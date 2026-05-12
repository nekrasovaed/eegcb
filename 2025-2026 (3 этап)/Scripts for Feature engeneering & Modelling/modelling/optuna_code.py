from __future__ import annotations
import json, datetime
from pathlib import Path
from typing import Optional

import numpy as np
import pandas as pd
from sklearn.model_selection import StratifiedGroupKFold
from sklearn.metrics import (
    f1_score, r2_score, mean_squared_error, mean_absolute_error,
    classification_report, confusion_matrix
)

from xgboost import XGBClassifier, XGBRegressor
from xgboost.core import XGBoostError
from catboost import CatBoostClassifier, CatBoostRegressor
import optuna

RANDOM_STATE = 1717


def _encode_multiclass_fold_labels(
    y_tr: pd.Series,
    y_va: pd.Series,
    y_es: pd.Series | None,
) -> tuple[pd.Series, pd.Series, pd.Series | None, int] | None:
    """
    Encode fold labels to contiguous [0..K-1] using train-fold classes only.
    Returns None when validation/ES contains unseen classes.
    """
    classes = pd.Index(pd.Series(y_tr).dropna().unique())
    if len(classes) < 2:
        return None

    class_to_idx = {cls: i for i, cls in enumerate(classes)}

    y_tr_enc = pd.Series(y_tr).map(class_to_idx)
    y_va_enc = pd.Series(y_va).map(class_to_idx)

    if y_tr_enc.isna().any() or y_va_enc.isna().any():
        return None

    y_tr_enc = y_tr_enc.astype(int)
    y_va_enc = y_va_enc.astype(int)

    y_es_enc = None
    if y_es is not None:
        y_es_enc = pd.Series(y_es).map(class_to_idx)
        if y_es_enc.isna().any():
            return None
        y_es_enc = y_es_enc.astype(int)

    return y_tr_enc, y_va_enc, y_es_enc, int(len(classes))


def _to_label_vector(y_pred) -> np.ndarray:
    """Convert model output to a 1D label vector."""
    arr = np.asarray(y_pred)
    if arr.ndim == 1:
        return arr
    if arr.ndim == 2:
        # Handle probability/indicator-like outputs.
        if arr.shape[1] == 1:
            return arr[:, 0]
        return arr.argmax(axis=1)
    return arr.reshape(-1)


# --------------------------
# Model search spaces
# --------------------------
def xgb_search_space(
    trial: optuna.Trial,
    gpu: bool,
    problem: str,
    tune_n_estimators: bool = False,
):
    params = {
        "booster": "gbtree",
        "tree_method": "gpu_hist" if gpu else "hist",
        "learning_rate": trial.suggest_float("learning_rate", 0.02, 0.2, log=True),
        "max_depth": trial.suggest_int("max_depth", 3, 8),
        "min_child_weight": trial.suggest_float("min_child_weight", 1.0, 10.0, log=True),
        "subsample": trial.suggest_float("subsample", 0.7, 1.0),
        "colsample_bytree": trial.suggest_float("colsample_bytree", 0.7, 1.0),
        "gamma": trial.suggest_float("gamma", 0.0, 2.0),
        "reg_alpha": trial.suggest_float("reg_alpha", 0.0001, 10.0, log=True),
        "reg_lambda": trial.suggest_float("reg_lambda", 0.0001, 10.0, log=True),
        "max_bin": 256,
        # tune only if ES is off
        "n_estimators": trial.suggest_int("n_estimators", 5, 500)
                        if tune_n_estimators else 1500,
        "verbosity": 0,
        "random_state": RANDOM_STATE,
        "n_jobs": -1,
    }

    if problem == "binary":
        params.update(objective="binary:logistic", eval_metric="logloss")
        return XGBClassifier(**params)
    elif problem == "multiclass":
        params.update(objective="multi:softprob", eval_metric="mlogloss")
        return XGBClassifier(**params)
    else:
        params.update(objective="reg:squarederror", eval_metric="rmse")
        return XGBRegressor(**params)


def catboost_search_space(
    trial: optuna.Trial,
    gpu: bool,
    problem: str,
    tune_n_estimators: bool = False,
):
    bootstrap_type = trial.suggest_categorical("bootstrap_type", ["Bayesian", "Bernoulli"])
    params = {
        "learning_rate": trial.suggest_float("learning_rate", 0.02, 0.2, log=True),
        "depth": trial.suggest_int("depth", 4, 8),
        "l2_leaf_reg": trial.suggest_float("l2_leaf_reg", 1.0, 20.0, log=True),
        "random_strength": trial.suggest_float("random_strength", 0.0, 1.0),
        "grow_policy": trial.suggest_categorical("grow_policy", ["SymmetricTree"]),
        "leaf_estimation_method": trial.suggest_categorical("leaf_estimation_method", ["Newton", "Gradient"]),
        "leaf_estimation_iterations": trial.suggest_int("leaf_estimation_iterations", 1, 5),
        "max_bin": 256,
        "bootstrap_type": bootstrap_type,
        "task_type": "GPU" if gpu else "CPU",
        # tune only if ES is off
        "iterations": trial.suggest_int("iterations", 5, 500)
                      if tune_n_estimators else 1500,
        "random_state": RANDOM_STATE,
        "verbose": False,
    }

    if bootstrap_type == "Bayesian":
        params["bagging_temperature"] = trial.suggest_float("bagging_temperature", 0.0, 5.0)
    else:
        params["subsample"] = trial.suggest_float("subsample", 0.6, 1.0)

    if problem == "binary":
        params["loss_function"] = "Logloss"
        return CatBoostClassifier(**params)
    elif problem == "multiclass":
        params["loss_function"] = "MultiClass"
        return CatBoostClassifier(**params)
    else:
        params["loss_function"] = "RMSE"
        return CatBoostRegressor(**params)


# --------------------------
# CV fit (selection CV)
# --------------------------
def fit_and_eval_cv(
    model_name: str,
    trial: optuna.Trial,
    problem: str,
    X_train: pd.DataFrame,
    y_train: pd.Series,
    strat_labels,
    groups,
    groups_mm,
    cv: int,
    use_early_stopping: bool = True,
    early_stopping_rounds: int = 100,
    gpu: bool = False,
    cat_cols: list[str] | None = None,
    use_min: bool = False,
    refit_cv: bool = False,
    cv_seed: int = RANDOM_STATE,  # <— seed for the selection CV
) -> float:
    skf = StratifiedGroupKFold(n_splits=cv, shuffle=True, random_state=cv_seed)
    folds = list(skf.split(X_train, strat_labels, groups))
    n_folds = len(folds)
    all_idx = np.arange(len(X_train))

    scores: list[float] = []

    for outer_fold in range(n_folds):
        _, va_idx_outer = folds[outer_fold]

        if use_early_stopping:
            es_fold = (outer_fold + 1) % n_folds
            _, es_idx = folds[es_fold]
            tr_idx = np.setdiff1d(all_idx, np.concatenate([va_idx_outer, es_idx]))
        else:
            es_idx = None
            tr_idx = np.setdiff1d(all_idx, va_idx_outer)

        if groups_mm is not None:
            tr_idx = [i for i in tr_idx if i in groups_mm[0]]
            va_idx = [i for i in va_idx_outer if i in groups_mm[1]]
            if es_idx is not None:
                es_idx = [i for i in es_idx if i in groups_mm[1]]
        else:
            va_idx = va_idx_outer

        X_tr, y_tr = X_train.iloc[tr_idx], y_train.iloc[tr_idx]
        X_va, y_va = X_train.iloc[va_idx], y_train.iloc[va_idx]

        if len(X_tr) == 0 or len(X_va) == 0:
            continue
        if problem != "regression" and pd.Series(y_tr).nunique(dropna=True) < 2:
            continue

        if use_early_stopping:
            if es_idx is None or len(es_idx) == 0:
                X_es, y_es = X_va, y_va
            else:
                X_es, y_es = X_train.iloc[es_idx], y_train.iloc[es_idx]
            if len(X_es) == 0:
                X_es, y_es = X_va, y_va
        else:
            X_es = y_es = None

        # fresh model per fold
        if model_name == "xgb":
            mdl = xgb_search_space(trial, gpu=gpu, problem=problem, tune_n_estimators=not use_early_stopping)
        elif model_name == "catboost":
            mdl = catboost_search_space(trial, gpu=gpu, problem=problem, tune_n_estimators=not use_early_stopping)
        else:
            raise ValueError(f"Unknown model_name: {model_name}")

        # fit
        if model_name == "xgb":
            y_tr_fit, y_va_fit = y_tr, y_va
            y_es_fit = y_es

            if problem == "multiclass":
                encoded = _encode_multiclass_fold_labels(y_tr=y_tr, y_va=y_va, y_es=y_es)
                if encoded is None:
                    continue
                y_tr_fit, y_va_fit, y_es_fit, num_class = encoded
                mdl.set_params(num_class=num_class)

            try:
                if use_early_stopping:
                    mdl.set_params(early_stopping_rounds=early_stopping_rounds)
                mdl.fit(
                    X_tr,
                    y_tr_fit,
                    eval_set=[(X_es, y_es_fit)] if use_early_stopping else None,
                    verbose=False,
                )
            except XGBoostError as e:
                if "num_class" in str(e):
                    continue
                raise

            if use_early_stopping and refit_cv:
                best_iter = getattr(mdl, "best_iteration", None)
                best_n_estimators = (best_iter + 1) if best_iter is not None else mdl.n_estimators
                refit_idx = np.concatenate([np.array(tr_idx), np.array(es_idx)]) if (es_idx is not None and len(es_idx) > 0) else np.array(tr_idx)
                mdl = xgb_search_space(trial, gpu=gpu, problem=problem, tune_n_estimators=False)
                if problem == "multiclass":
                    mdl.set_params(num_class=int(pd.Series(y_train.iloc[refit_idx]).nunique(dropna=True)))
                mdl.set_params(n_estimators=best_n_estimators)
                y_refit = y_train.iloc[refit_idx]
                if problem == "multiclass":
                    y_refit = pd.Series(y_refit).astype("category").cat.codes
                try:
                    mdl.fit(X_train.iloc[refit_idx], y_refit, verbose=False)
                except XGBoostError as e:
                    if "num_class" in str(e):
                        continue
                    raise

            y_pred = _to_label_vector(mdl.predict(X_va))
            if problem == "multiclass":
                classes = pd.Index(pd.Series(y_tr).dropna().unique())
                y_pred = classes.take(np.asarray(y_pred, dtype=int))

        elif model_name == "catboost":
            mdl.fit(
                X_tr, y_tr,
                eval_set=(X_es, y_es) if use_early_stopping else None,
                cat_features=cat_cols if cat_cols else None,
                use_best_model=True,
                early_stopping_rounds=early_stopping_rounds if use_early_stopping else None,
                verbose=False,
            )

            if use_early_stopping and refit_cv:
                try:
                    best_iter = mdl.get_best_iteration()
                except Exception:
                    best_iter = None
                best_iterations = (best_iter + 1) if (best_iter is not None and best_iter >= 0) else mdl.tree_count_
                refit_idx = np.concatenate([np.array(tr_idx), np.array(es_idx)]) if (es_idx is not None and len(es_idx) > 0) else np.array(tr_idx)
                mdl = catboost_search_space(trial, gpu=gpu, problem=problem, tune_n_estimators=False)
                mdl.set_params(iterations=best_iterations)
                mdl.fit(X_train.iloc[refit_idx], y_train.iloc[refit_idx],
                        cat_features=cat_cols if cat_cols else None, verbose=False)

            y_pred = _to_label_vector(mdl.predict(X_va))

        # metric
        if problem != "regression":
            score = f1_score(y_va, y_pred, average="macro", zero_division=0)
        else:
            score = r2_score(y_va, y_pred)

        scores.append(score)

    if len(scores) == 0:
        raise optuna.TrialPruned("No valid CV folds after filtering/encoding.")

    scores = np.array(scores, dtype=float)
    return float(scores.min() if use_min else scores.mean())


# --------------------------
# Fresh-folds evaluation (frozen best params)
# --------------------------
def eval_fresh_folds(
    model_name: str,
    best_params: dict,
    problem: str,
    X_train: pd.DataFrame,
    y_train: pd.Series,
    strat_labels,
    groups,
    groups_mm,
    *,
    cv: int,
    use_early_stopping: bool,
    early_stopping_rounds: int,
    gpu: bool,
    cat_cols: list[str] | None,
    refit_cv: bool,
    cv_seed: int,
) -> dict:
    """Evaluate a *fixed* hyperparameter set on a fresh StratifiedGroupKFold."""
    skf = StratifiedGroupKFold(n_splits=cv, shuffle=True, random_state=cv_seed)
    folds = list(skf.split(X_train, strat_labels, groups))
    n_folds = len(folds)
    all_idx = np.arange(len(X_train))

    scores: list[float] = []

    fixed = optuna.trial.FixedTrial(best_params)

    for outer_fold in range(n_folds):
        _, va_idx_outer = folds[outer_fold]

        if use_early_stopping:
            es_fold = (outer_fold + 1) % n_folds
            _, es_idx = folds[es_fold]
            tr_idx = np.setdiff1d(all_idx, np.concatenate([va_idx_outer, es_idx]))
        else:
            es_idx = None
            tr_idx = np.setdiff1d(all_idx, va_idx_outer)

        if groups_mm is not None:
            tr_idx = [i for i in tr_idx if i in groups_mm[0]]
            va_idx = [i for i in va_idx_outer if i in groups_mm[1]]
            if es_idx is not None:
                es_idx = [i for i in es_idx if i in groups_mm[1]]
        else:
            va_idx = va_idx_outer

        X_tr, y_tr = X_train.iloc[tr_idx], y_train.iloc[tr_idx]
        X_va, y_va = X_train.iloc[va_idx], y_train.iloc[va_idx]

        if len(X_tr) == 0 or len(X_va) == 0:
            continue
        if problem != "regression" and pd.Series(y_tr).nunique(dropna=True) < 2:
            continue

        if use_early_stopping:
            if es_idx is None or len(es_idx) == 0:
                X_es, y_es = X_va, y_va
            else:
                X_es, y_es = X_train.iloc[es_idx], y_train.iloc[es_idx]
            if len(X_es) == 0:
                X_es, y_es = X_va, y_va
        else:
            X_es = y_es = None

        # instantiate with fixed params
        if model_name == "xgb":
            mdl = xgb_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=not use_early_stopping)
            y_tr_fit, y_va_fit = y_tr, y_va
            y_es_fit = y_es

            if problem == "multiclass":
                encoded = _encode_multiclass_fold_labels(y_tr=y_tr, y_va=y_va, y_es=y_es)
                if encoded is None:
                    continue
                y_tr_fit, y_va_fit, y_es_fit, num_class = encoded
                mdl.set_params(num_class=num_class)

            try:
                if use_early_stopping:
                    mdl.set_params(early_stopping_rounds=early_stopping_rounds)
                mdl.fit(
                    X_tr,
                    y_tr_fit,
                    eval_set=[(X_es, y_es_fit)] if use_early_stopping else None,
                    verbose=False,
                )
            except XGBoostError as e:
                if "num_class" in str(e):
                    continue
                raise

            if use_early_stopping and refit_cv:
                best_iter = getattr(mdl, "best_iteration", None)
                best_n_estimators = (best_iter + 1) if best_iter is not None else mdl.n_estimators
                refit_idx = np.concatenate([np.array(tr_idx), np.array(es_idx)]) if (es_idx is not None and len(es_idx) > 0) else np.array(tr_idx)
                mdl = xgb_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=False)
                if problem == "multiclass":
                    mdl.set_params(num_class=int(pd.Series(y_train.iloc[refit_idx]).nunique(dropna=True)))
                mdl.set_params(n_estimators=best_n_estimators)
                y_refit = y_train.iloc[refit_idx]
                if problem == "multiclass":
                    y_refit = pd.Series(y_refit).astype("category").cat.codes
                try:
                    mdl.fit(X_train.iloc[refit_idx], y_refit, verbose=False)
                except XGBoostError as e:
                    if "num_class" in str(e):
                        continue
                    raise

            y_pred = _to_label_vector(mdl.predict(X_va))
            if problem == "multiclass":
                y_pred = pd.Index(pd.Series(y_tr).dropna().unique()).take(np.asarray(y_pred, dtype=int))

        elif model_name == "catboost":
            mdl = catboost_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=not use_early_stopping)
            mdl.fit(
                X_tr, y_tr,
                eval_set=(X_es, y_es) if use_early_stopping else None,
                cat_features=cat_cols if cat_cols else None,
                use_best_model=True,
                early_stopping_rounds=early_stopping_rounds if use_early_stopping else None,
                verbose=False,
            )

            if use_early_stopping and refit_cv:
                try:
                    best_iter = mdl.get_best_iteration()
                except Exception:
                    best_iter = None
                best_iterations = (best_iter + 1) if (best_iter is not None and best_iter >= 0) else mdl.tree_count_
                refit_idx = np.concatenate([np.array(tr_idx), np.array(es_idx)]) if (es_idx is not None and len(es_idx) > 0) else np.array(tr_idx)
                mdl = catboost_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=False)
                mdl.set_params(iterations=best_iterations)
                mdl.fit(X_train.iloc[refit_idx], y_train.iloc[refit_idx],
                        cat_features=cat_cols if cat_cols else None, verbose=False)

            y_pred = _to_label_vector(mdl.predict(X_va))

        # metric
        if problem != "regression":
            score = f1_score(y_va, y_pred, average="macro", zero_division=0)
        else:
            score = r2_score(y_va, y_pred)
        scores.append(float(score))

    if len(scores) == 0:
        raise ValueError("No valid fresh CV folds after filtering/encoding.")

    scores = np.array(scores, dtype=float)
    return {
        "mean": float(scores.mean()),
        "std": float(scores.std(ddof=1)) if len(scores) > 1 else 0.0,
        "median": float(np.median(scores)),
        "min": float(scores.min()),
        "per_fold": scores.tolist(),
        "cv": int(cv),
        "seed": int(cv_seed),
    }


# --------------------------
# Study runner (Optuna)
# --------------------------
def run_study(
    model_name: str,
    X_train: pd.DataFrame,
    n_trials: int,
    problem: str,
    y_train: pd.Series,
    strat_train: pd.Series,
    groups,
    groups_mm,
    use_early_stopping=True,
    cv = 5,
    use_min=False,
    refit_cv: bool = False,
    cat_cols: list[str] | None = None,
    cv_seed: int = RANDOM_STATE,   # seed for the selection CV
) -> optuna.Study:

    direction = "maximize"

    def objective(trial: optuna.Trial) -> float:
        score = fit_and_eval_cv(
            model_name=model_name,
            trial=trial,
            problem=problem,
            X_train=X_train,
            y_train=y_train,
            strat_labels=strat_train,
            groups=groups,
            groups_mm=groups_mm,
            use_early_stopping=use_early_stopping,
            cv=cv,
            use_min=use_min,
            refit_cv=refit_cv,
            cat_cols=cat_cols,
            cv_seed=cv_seed,
        )
        trial.set_user_attr("score", score)
        return score

    sampler = optuna.samplers.TPESampler(seed=RANDOM_STATE)
    study = optuna.create_study(study_name=f'{problem}_{model_name}',
                                direction=direction, sampler=sampler)
    study.optimize(objective, n_trials=n_trials, show_progress_bar=True)
    print(f"\nBest model ({model_name}) CV score: {study.best_value:.5f}")
    print("Best params:", study.best_trial.params)
    return study


# --------------------------
# Final refit on full train + test eval
# --------------------------
def fit_best_and_test(
    model_name: str,
    best_params: dict,
    problem: str,  # "binary" | "multiclass" | "regression"
    X_train: pd.DataFrame,
    X_test: pd.DataFrame,
    y_train: pd.Series,
    y_test: pd.Series,
    strat_train: np.ndarray,
    groups: np.ndarray,
    groups_mm: np.ndarray | None,
    use_early_stopping = True,
    early_stopping_rounds: int = 100,
    target_names: list[str] | None = None,
    cat_cols: list[str] | None = None,
    gpu: bool = False,
    refit_test: bool = True,
) -> dict:

    if use_early_stopping:
        skf = StratifiedGroupKFold(n_splits=4, shuffle=True, random_state=RANDOM_STATE + 111)
        for _, (tr_idx, va_idx) in enumerate(skf.split(X_train, strat_train, groups), 1):
            break
        if groups_mm is not None:
            tr_idx = [i for i in tr_idx if i in groups_mm[0]]
            va_idx = [i for i in va_idx if i in groups_mm[1]]
        X_tr_es, X_va_es = X_train.iloc[tr_idx], X_train.iloc[va_idx]
        y_tr_es, y_va_es = y_train.iloc[tr_idx], y_train.iloc[va_idx]
    else:
        X_tr_es = X_va_es = y_tr_es = y_va_es = None

    X_full, y_full = X_train, y_train

    if model_name == "xgb":
        fixed = optuna.trial.FixedTrial(best_params)
        if use_early_stopping:
            es_model = xgb_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=False)
            es_model.set_params(early_stopping_rounds=early_stopping_rounds)
            es_model.fit(X_tr_es, y_tr_es, eval_set=[(X_va_es, y_va_es)], verbose=False)
            best_iter = getattr(es_model, "best_iteration", None)
            best_n_estimators = (best_iter + 1) if best_iter is not None else es_model.n_estimators
            model = es_model if not refit_test else xgb_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=False)
            if refit_test:
                model.set_params(n_estimators=best_n_estimators)
                model.fit(X_full, y_full, verbose=False)
        else:
            model = xgb_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=True)
            model.fit(X_full, y_full, verbose=False)

    elif model_name == "catboost":
        fixed = optuna.trial.FixedTrial(best_params)
        if use_early_stopping:
            es_model = catboost_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=False)
            es_model.fit(
                X_tr_es, y_tr_es,
                eval_set=(X_va_es, y_va_es),
                cat_features=cat_cols if cat_cols else None,
                use_best_model=True,
                early_stopping_rounds=early_stopping_rounds,
                verbose=False,
            )
            try:
                best_iter = es_model.get_best_iteration()
            except Exception:
                best_iter = None
            best_iterations = (best_iter + 1) if (best_iter is not None and best_iter > 0) else es_model.tree_count_
            model = es_model if not refit_test else catboost_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=False)
            if refit_test:
                model.set_params(iterations=best_iterations)
                model.fit(X_full, y_full, cat_features=cat_cols if cat_cols else None, verbose=False)
        else:
            model = catboost_search_space(fixed, gpu=gpu, problem=problem, tune_n_estimators=True)
            model.fit(X_full, y_full, cat_features=cat_cols if cat_cols else None, verbose=False)

    else:
        raise ValueError("Unsupported model type.")

    y_pred_test = model.predict(X_test)

    if problem != "regression":
        f1_macro = f1_score(y_test, y_pred_test, average="macro", zero_division=0)
        # participant-level aggregation
        try:
            y_pred_df = pd.Series(y_pred_test, index=X_test.index, name='preds').to_frame()
        except:
            y_pred_df = pd.DataFrame(y_pred_test[:,0], index=X_test.index, columns=['preds'])
        y_pred_df['participant'] = [int(i.split('_')[0]) for i in X_test.index]
        y_test_df = y_test.to_frame(name='trues')
        y_test_df['participant'] = [int(i.split('_')[0]) for i in X_test.index]
        participant_preds = y_pred_df.groupby('participant')['preds'].agg(lambda s: s.mode().iloc[0])
        participant_trues = y_test_df.groupby('participant')['trues'].agg(lambda s: s.mode().iloc[0])
        f1_macro_participants = f1_score(participant_trues, participant_preds, average="macro", zero_division=0)

        print(f"\n[{model_name}] Test macro-F1: {f1_macro:.5f}")
        print(classification_report(y_test, y_pred_test, target_names=target_names, zero_division=0))
        print("Confusion matrix:\n", confusion_matrix(y_test, y_pred_test))

        return {
            "model": model,
            "test_score": float(f1_macro),
            "test_score_participants": float(f1_macro_participants),
            "y_pred_test": y_pred_test,
            "report": classification_report(y_test, y_pred_test, output_dict=True, zero_division=0),
            "conf_matrix": confusion_matrix(y_test, y_pred_test),
        }
    else:
        r2  = float(r2_score(y_test, y_pred_test))
        rmse = float(np.sqrt(mean_squared_error(y_test, y_pred_test)))
        mae = float(mean_absolute_error(y_test, y_pred_test))
        print(f"\n[{model_name}] Test R²: {r2:.5f} | RMSE: {rmse:.5f} | MAE: {mae:.5f}")

        return {
            "model": model,
            "test_score": r2,
            "rmse": rmse,
            "mae": mae,
            "y_pred_test": y_pred_test,
            "report": None,
            "conf_matrix": None,
        }


# --------------------------
# Orchestrator
# --------------------------
def run_and_log(
    train_features: str,
    problem: str,
    X_train: pd.DataFrame,
    X_test: pd.DataFrame,
    y_train: pd.Series,
    y_test: pd.Series,
    strat_train: np.ndarray,
    groups,
    groups_mm: np.ndarray | None = None,
    *,
    use_early_stopping = True,
    refit_cv: bool = False,
    refit_test: bool = True,
    n_trials_xgb: int = 50,
    n_trials_cb: int = 50,
    cv: int = 4,
    gpu: bool = False,
    use_min = False,
    cat_cols: list[str] | None = None,
    target_names: list[str] | None = None,
    out_path: str | Path | None = None,
    # NEW: fresh-folds evaluation controls
    fresh_eval: bool = True,
    fresh_cv: int = 4,
    fresh_seed: int = RANDOM_STATE + 777,
    selection_cv_seed: int = RANDOM_STATE,
) -> dict:

    if refit_cv or refit_test:
        assert use_early_stopping, "Refit makes sense only when using early stopping."

    ts = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

    # ---- Run studies (ONLY XGB + CATBOOST)
    study_xgb = best_xgb = None
    xgb_error: str | None = None
    try:
        study_xgb = run_study(
            "xgb", X_train=X_train, n_trials=n_trials_xgb,
            problem=problem, y_train=y_train, strat_train=strat_train, groups=groups, groups_mm=groups_mm,
            cv=cv, use_early_stopping=use_early_stopping, use_min=use_min, refit_cv=refit_cv, cat_cols=cat_cols,
            cv_seed=selection_cv_seed,
        )
        best_xgb = fit_best_and_test(
            "xgb", study_xgb.best_trial.params, problem,
            X_train=X_train, X_test=X_test, y_train=y_train, strat_train=strat_train, y_test=y_test,
            groups=groups, groups_mm=groups_mm, use_early_stopping=use_early_stopping,
            target_names=target_names, cat_cols=cat_cols, gpu=gpu, refit_test=refit_test
        )
    except (XGBoostError, ValueError, optuna.exceptions.TrialPruned) as e:
        xgb_error = str(e)
        print(f"[xgb] skipped due to error: {xgb_error}")

    study_cb = run_study(
        "catboost", X_train=X_train, n_trials=n_trials_cb,
        problem=problem, y_train=y_train, strat_train=strat_train, groups=groups, groups_mm=groups_mm,
        cv=cv, use_early_stopping=use_early_stopping, use_min=use_min, refit_cv=refit_cv, cat_cols=cat_cols,
        cv_seed=selection_cv_seed,
    )

    # ---- Refit best and evaluate on TEST
    best_cb = fit_best_and_test(
        "catboost", study_cb.best_trial.params, problem,
        X_train=X_train, X_test=X_test, y_train=y_train, strat_train=strat_train, y_test=y_test,
        groups=groups, groups_mm=groups_mm, use_early_stopping=use_early_stopping,
        target_names=target_names, cat_cols=cat_cols, gpu=gpu, refit_test=refit_test
    )

    # ---- Fresh-folds evaluation (frozen params on new folds)
    fresh_xgb = fresh_cb = None
    if fresh_eval:
        if study_xgb is not None:
            try:
                fresh_xgb = eval_fresh_folds(
                    "xgb", study_xgb.best_trial.params, problem,
                    X_train, y_train, strat_train, groups, groups_mm,
                    cv=fresh_cv, use_early_stopping=use_early_stopping,
                    early_stopping_rounds=100, gpu=gpu, cat_cols=cat_cols,
                    refit_cv=refit_cv, cv_seed=fresh_seed,
                )
            except (XGBoostError, ValueError) as e:
                xgb_error = str(e) if xgb_error is None else xgb_error
        fresh_cb = eval_fresh_folds(
            "catboost", study_cb.best_trial.params, problem,
            X_train, y_train, strat_train, groups, groups_mm,
            cv=fresh_cv, use_early_stopping=use_early_stopping,
            early_stopping_rounds=100, gpu=gpu, cat_cols=cat_cols,
            refit_cv=refit_cv, cv_seed=fresh_seed,
        )

    # ---- Pack results
    def pack_block(study, best, fresh_stats, error_message: str | None = None):
        if study is None or best is None:
            return {
                "cv_best_value": None,
                "cv_best_trial_number": None,
                "cv_best_duration": None,
                "suggested_params": {},
                "full_model_params": {},
                "test_metrics": {"primary": float("nan")},
                "error": error_message,
            }
        mdl = best["model"]
        try:
            full_params = mdl.get_params(deep=True)
        except Exception:
            full_params = {k: getattr(mdl, k) for k in dir(mdl) if not k.startswith("_")}

        block = {
            "cv_best_value": float(study.best_value),
            "cv_best_trial_number": int(study.best_trial.number),
            "cv_best_duration": str(study.best_trial.duration),
            "suggested_params": study.best_trial.params,
            "full_model_params": full_params,
            "test_metrics": {
                "primary": float(best["test_score"]),
                **({"rmse": float(best["rmse"]), "mae": float(best["mae"])} if "rmse" in best else {})
            },
        }
        if best.get("report") is not None:
            block["classification_report"] = best["report"]
        if best.get("conf_matrix") is not None:
            block["confusion_matrix"] = np.asarray(best["conf_matrix"]).tolist()
        if fresh_stats is not None:
            block["fresh_cv"] = fresh_stats
        return block

    results = {
        "meta": {
            "timestamp": ts,
            "train_features": train_features,
            "problem": problem,
            "selection_cv_folds": cv,
            "selection_cv_seed": selection_cv_seed,
            "fresh_eval": bool(fresh_eval),
            "fresh_cv_folds": int(fresh_cv),
            "fresh_cv_seed": int(fresh_seed),
            "gpu": gpu,
            "n_trials": {"xgb": n_trials_xgb, "catboost": n_trials_cb},
            "target_names": target_names,
        },
        "xgb": pack_block(study_xgb, best_xgb, fresh_xgb, error_message=xgb_error),
        "catboost": pack_block(study_cb, best_cb, fresh_cb),
    }

    # ---- Save
    if out_path is None:
        folder = "optuna_results" if groups_mm is None else "optuna_results_mm"
        out_path = Path(
            f"{folder}/{train_features}.json"
        )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(results, f)
    print(f"\nSaved results to: {out_path}")
    return results

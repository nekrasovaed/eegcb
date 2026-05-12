from __future__ import annotations

import itertools
import json
from pathlib import Path
from typing import Iterable, List, Tuple, Dict, Any, Optional, Union, Sequence

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split

from .optuna_code import run_and_log                 # your pipeline (XGB + CatBoost only)
from .read_data import get_data_for_split            # your loader with stimulus-normalised keys

RANDOM_STATE = 1717

XName = Union[str, Tuple[str, ...]]
FeatureKey = Union[str, Tuple[str, ...]]


def run_optuna_grid(
    X_names: Iterable[XName],
    targets: Iterable[str],             # {"cognitive_bias","match_mismatch"}
    splits: Iterable[str],              # {"binary","multiclass","regression"}
    feature_keys: Iterable[FeatureKey], # e.g. ["all_features", ("participants_normalised_features","stimulus_normalised_features")]
    *,
    name_prefix: str = "exp",
    append_cols_from_stimuli: Optional[List[str]] = None,  # extra cols besides auto 'valence' for CB
    clean_train: bool = True,                              # drop NaN columns based on TRAIN only
    early_stopping_grid: Tuple[bool, ...] = (True,),
    refit_cv_grid: Tuple[bool, ...] = (False, True),
    refit_test_grid: Tuple[bool, ...] = (False,),
    use_min_grid: Tuple[bool, ...] = (False,),
    cv: int = 5,
    gpu: bool = False,
    n_trials_xgb: int = 50,
    n_trials_cb: int = 50,
) -> pd.DataFrame:
    """
    Grid over X_name × target × split × feature_key × {ES, refit_cv, refit_test, use_min}.
    - X_name may be a string or a tuple of strings (concatenate sources).
    - feature_key may be a string or a tuple of strings (concatenate feature sets).
    - For 'cognitive_bias', 'valence' is always appended to features and passed in cat_cols.
    - For 'match_mismatch', groups_mm is always built 50/50 by TextID, stratified by valence.
    Returns a summary DataFrame with columns: file, model, test_quality, train_features.
    """
    extra_cols = list(append_cols_from_stimuli or [])
    rows: List[Dict[str, Any]] = []

    for X_name, target, split in itertools.product(X_names, targets, splits):

        # choose problem and y-key
        if split == "regression":
            problem_key = "regression"   # y_reg
            problem = "regression"
        else:
            problem_key = "cb"           # y_{binary|multiclass} under 'cb'
            problem = "binary" if split == "binary" else "multiclass"

        # enforce valence for cognitive_bias
        enforced_cols = list(dict.fromkeys(extra_cols + (["valence"] if target == "cognitive_bias" else [])))

        for feat_key in feature_keys:
            try:
                (
                    base_tr, base_te,         # concatenated over sources AND feature sets
                    stim_tr, stim_te,         # stimuli frames aligned to base indices
                    y_train, y_test,          # targets aligned to base indices
                    groups,                   # participants (strings)
                    merged_label,             # "screen" or "screen+answer"
                    feat_label,               # "all_features" or "a+b"
                ) = _load_concat_features_for_key(
                    X_name=X_name,
                    target=target,
                    split=split,
                    feat_key=feat_key,
                    problem_key=problem_key,
                )
            except KeyError:
                # some requested feature set missing in one of the sources
                continue

            # build groups_mm for match_mismatch on the (possibly concatenated) TRAIN block
            if target == "match_mismatch" or target=="match_mismatch_general":
                groups_mm = _build_groups_mm_for_mm(
                    X_train_index=base_tr.index,
                    stim_train_df=stim_tr,
                )
            else:
                groups_mm = None

            for early in early_stopping_grid:
                this_refit_cv   = refit_cv_grid   if early else (False,)
                this_refit_test = refit_test_grid if early else (False,)

                for refit_cv in this_refit_cv:
                    for refit_test in this_refit_test:
                        for use_min in use_min_grid:
                            X_train, X_test, cat_cols = _prepare_X_block(
                                base_tr, base_te, stim_tr, stim_te,
                                add_cols=enforced_cols,
                                clean=clean_train,
                            )

                            train_features_name = _build_name(
                                prefix=name_prefix,
                                axes={"X_name": merged_label, "target": target, "problem": split},
                                base_key=feat_label,
                                add_cols=enforced_cols,
                                clean=clean_train,
                                use_min=use_min,
                                early=early,
                                refit_cv=refit_cv,
                                refit_test=refit_test,
                            )

                            base_dir = "optuna_results_mm" if groups_mm is not None else "optuna_results"
                            json_path = Path(f"{base_dir}/{train_features_name}.json")

                            if json_path.exists():
                                with open(json_path, encoding="utf-8") as f:
                                    results = json.load(f)
                                json_path_str = json_path.as_posix()
                                rows.append({
                                    "file": json_path_str,
                                    "model": "xgb",
                                    "test_quality": float(results["xgb"]["test_metrics"]["primary"]),
                                    "cv_score": float(results["xgb"]["cv_best_value"]),
                                })
                                rows.append({
                                    "file": json_path_str,
                                    "model": "catboost",
                                    "test_quality": float(results["catboost"]["test_metrics"]["primary"]),
                                    "cv_score": float(results["catboost"]["cv_best_value"]),
                                })
                                continue

                            results = run_and_log(
                                train_features=train_features_name,
                                problem=problem,
                                X_train=X_train,
                                X_test=X_test,
                                y_train=y_train,
                                y_test=y_test,
                                strat_train=y_train,     # your convention
                                groups=groups,
                                groups_mm=groups_mm,
                                use_early_stopping=early,
                                refit_cv=refit_cv,
                                refit_test=refit_test,
                                n_trials_xgb=n_trials_xgb,
                                n_trials_cb=n_trials_cb,
                                cv=cv,
                                gpu=gpu,
                                use_min=use_min,
                                cat_cols=cat_cols,
                                target_names=None,
                                out_path=None,
                            )

                            json_path_str = json_path.as_posix()
                            rows.append({
                                "file": json_path_str,
                                "model": "xgb",
                                "test_quality": float(results["xgb"]["test_metrics"]["primary"]),
                                "cv_score": float(results["xgb"]["cv_best_value"]),
                            })
                            rows.append({
                                "file": json_path_str,
                                "model": "catboost",
                                "test_quality": float(results["catboost"]["test_metrics"]["primary"]),
                                "cv_score": float(results["catboost"]["cv_best_value"]),
                            })

    return pd.DataFrame(rows, columns=["file", "model", "test_quality", "cv_score"])


# ----------------------------- helpers -----------------------------

def _as_tuple(x: XName) -> Tuple[str, ...]:
    return x if isinstance(x, tuple) else (x,)


def _feat_list(k: FeatureKey) -> Tuple[str, ...]:
    return k if isinstance(k, tuple) else (k,)


def _merged_label(x: XName) -> str:
    xs = _as_tuple(x)
    return xs[0] if len(xs) == 1 else "+".join(xs)


def _feat_label(k: FeatureKey) -> str:
    ks = _feat_list(k)
    return ks[0] if len(ks) == 1 else "+".join(ks)


def _load_concat_features_for_key(
    *,
    X_name: XName,
    target: str,
    split: str,
    feat_key: FeatureKey,
    problem_key: str,  # "cb" or "regression"
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.Series, pd.Series, List[str], str, str]:
    """
    Load one or multiple X sources, and within each source concatenate one or multiple feature sets.
    Column names get a double prefix:  {source}__{featkey}__{col}
    Indices are intersected:
        - within source across feature sets
        - across sources
    Returns:
        base_tr, base_te, stim_tr, stim_te, y_train, y_test, groups, merged_label, feat_label
    """
    srcs = _as_tuple(X_name)
    feat_keys = _feat_list(feat_key)

    per_src_blocks: List[Tuple[str, Dict, Dict, pd.DataFrame, pd.DataFrame]] = []
    common_train: Optional[pd.Index] = None
    common_test: Optional[pd.Index] = None

    # 1) load each source and build concatenated blocks per source
    for s in srcs:
        df, tgt, _groups = get_data_for_split(X_name=s, target=target, split=split)

        # ensure all feature sets exist for this source
        for k in feat_keys:
            if k not in df:
                raise KeyError(f"{k} not present for source '{s}' / target '{target}' / split '{split}'")

        # index intersection within this source across requested feature sets
        train_idxs = [df[k]["X_train"].index for k in feat_keys]
        test_idxs  = [df[k]["X_test"].index  for k in feat_keys]
        src_train_inter = _intersect_indices(train_idxs)
        src_test_inter  = _intersect_indices(test_idxs)

        # concat feature sets with double prefix
        tr_parts, te_parts = [], []
        for k in feat_keys:
            tr_parts.append(df[k]["X_train"].reindex(src_train_inter).add_prefix(f"{s}__{k}__"))
            te_parts.append(df[k]["X_test"].reindex(src_test_inter).add_prefix(f"{s}__{k}__"))
        src_tr = pd.concat(tr_parts, axis=1)
        src_te = pd.concat(te_parts, axis=1)

        per_src_blocks.append((s, df, tgt, src_tr, src_te))

        # accumulate global intersections across sources
        common_train = src_tr.index if common_train is None else common_train.intersection(src_tr.index)
        common_test  = src_te.index  if common_test  is None else common_test.intersection(src_te.index)

    assert common_train is not None and common_test is not None

    # 2) align all blocks to global intersections and concat across sources
    tr_blocks, te_blocks = [], []
    for s, df, tgt, src_tr, src_te in per_src_blocks:
        tr_blocks.append(src_tr.reindex(common_train))
        te_blocks.append(src_te.reindex(common_test))

    base_tr = pd.concat(tr_blocks, axis=1)
    base_te = pd.concat(te_blocks, axis=1)

    # 3) stimuli and targets from first source, aligned to intersections
    first_df, first_tgt = per_src_blocks[0][1], per_src_blocks[0][2]
    stim_tr_full = first_df["stimuli_features"]["X_train"].reindex(common_train)
    stim_te_full = first_df["stimuli_features"]["X_test"].reindex(common_test)

    if problem_key == "regression":
        y_train = first_tgt["regression"]["y_train"].reindex(common_train)
        y_test  = first_tgt["regression"]["y_test"].reindex(common_test)
    else:
        y_train = first_tgt["cb"]["y_train"].reindex(common_train)
        y_test  = first_tgt["cb"]["y_test"].reindex(common_test)

    # 4) groups from base indices (participant id from index)
    groups = [str(idx).split("_")[0] for idx in base_tr.index]

    return base_tr, base_te, stim_tr_full, stim_te_full, y_train, y_test, groups, _merged_label(X_name), _feat_label(feat_key)


def _intersect_indices(indices: Sequence[pd.Index]) -> pd.Index:
    inter = indices[0]
    for idx in indices[1:]:
        inter = inter.intersection(idx)
    return inter


def _prepare_X_block(
    base_tr: pd.DataFrame,
    base_te: pd.DataFrame,
    stim_tr: pd.DataFrame,
    stim_te: pd.DataFrame,
    *,
    add_cols: List[str],
    clean: bool,
) -> tuple[pd.DataFrame, pd.DataFrame, List[str]]:
    """Join selected stimulus columns to base features and optionally drop NaN columns based on TRAIN only."""
    X_train = base_tr.copy()
    X_test  = base_te.copy()
    cat_cols: List[str] = []

    if add_cols:
        missing = [c for c in add_cols if c not in stim_tr.columns]
        if missing:
            raise KeyError(f"Missing stimulus columns: {missing}")
        X_train = X_train.join(stim_tr[add_cols])
        X_test  = X_test.join(stim_te[add_cols])
        cat_cols = [c for c in add_cols if c in X_train.columns]

    if clean:
        keep = X_train.columns[X_train.notna().all(axis=0)]
        X_train = X_train[keep]
        X_test  = X_test.reindex(columns=keep)
        cat_cols = [c for c in cat_cols if c in keep]

    return X_train, X_test, cat_cols


def _build_groups_mm_for_mm(
    X_train_index: pd.Index,
    stim_train_df: pd.DataFrame,
) -> List[List[int]]:
    """
    Build groups_mm for match_mismatch:
      - split unique TextID into train/val (50/50) stratified by valence
      - groups_mm[0] = positions with TextID in train_stimuli
        groups_mm[1] = positions with TextID in val_stimuli
    """
    st = stim_train_df.rename(columns={"Valence": "valence"})
    df_ids = st.copy()

    if "TextID" not in df_ids.columns:
        text_ids = [int(str(idx).split("_")[1]) for idx in df_ids.index]
        df_ids = df_ids.assign(TextID=text_ids)

    if "valence" not in df_ids.columns:
        raise KeyError("Stimulus features must contain 'valence' for match_mismatch.")

    uniq = df_ids[["TextID", "valence"]].drop_duplicates("TextID").dropna()

    
    train_stimuli, val_stimuli = train_test_split(
            uniq["TextID"].to_list(),
            test_size=0.5,
            stratify=uniq["valence"].to_list(),
            random_state=RANDOM_STATE,
        )


    train_set = set(int(x) for x in train_stimuli)
    val_set   = set(int(x) for x in val_stimuli)

    def _textid_from_index(idx: str) -> int:
        return int(str(idx).split("_")[1])

    g_train = [n for n, idx in enumerate(X_train_index) if _textid_from_index(idx) in train_set]
    g_val   = [n for n, idx in enumerate(X_train_index) if _textid_from_index(idx) in val_set]
    return [g_train, g_val]


def _build_name(
    *,
    prefix: str,
    axes: Dict[str, str],
    base_key: str,
    add_cols: List[str],
    clean: bool,
    use_min: bool,
    early: bool,
    refit_cv: bool,
    refit_test: bool,
) -> str:
    parts = [prefix]
    parts.extend([f"{k}={v}" for k, v in axes.items()])
    parts.append(f"feat={base_key}")
    if add_cols:
        parts.append("add=" + "+".join(add_cols))
    if clean:
        parts.append("clean")
    if use_min:
        parts.append("usemin")
    if early:
        parts.append("ES")
    if refit_cv:
        parts.append("refitCV")
    if refit_test:
        parts.append("refitTEST")
    return "__".join(parts)

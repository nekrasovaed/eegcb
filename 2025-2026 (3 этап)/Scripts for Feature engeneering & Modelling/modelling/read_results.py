import json
from pathlib import Path
import pandas as pd

def collect_optuna_results(base_dirs=("optuna_results", "optuna_results_mm")) -> pd.DataFrame:
    """
    Read Optuna result JSONs and return a DataFrame with:
      - name_of_experiment: file name (e.g., 'participants_binary.json')
      - model: one of ['xgb','lgbm','catboost'] if present in the file
      - test_quality: value from results[model]['test_metrics']['primary']
    """
    rows = []
    patterns = ("*.json", "*.csv.json")  # handle your earlier .csv.json saves too
    model_keys = ("xgb", "lgbm", "catboost")

    for base in base_dirs:
        base_path = Path(base)
        if not base_path.exists():
            continue

        files = []
        for pat in patterns:
            files.extend(base_path.rglob(pat))

        for fp in files:
            try:
                data = json.loads(fp.read_text(encoding="utf-8"))
            except Exception:
                continue  # skip unreadable files

            for mk in model_keys:
                block = data.get(mk)
                if not isinstance(block, dict):
                    continue
                test_metrics = block.get("test_metrics", {})
                test_quality = test_metrics.get("primary", None)
                if test_quality is None:
                    continue
                rows.append({
                    "name_of_experiment": fp.name,
                    "model": mk,
                    "test_quality": float(test_quality),
                })

    df = pd.DataFrame(rows, columns=["name_of_experiment", "model", "test_quality"])
    return df.sort_values(["name_of_experiment", "model"]).reset_index(drop=True)

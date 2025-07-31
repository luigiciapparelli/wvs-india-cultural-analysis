import pandas as pd
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import seaborn as sns

# Only load needed columns
usecols = [
    "future_respect_rev", "conf_police_rev", "conf_gov_rev", "follow_instr_rev",
    "conf_polpart_rev", "conf_arm_rev", "conf_majorcomp_rev", "unequal_power_rev"
]

# Load only relevant columns
df = pd.read_csv("wvs_all_data.csv", usecols=usecols)

# Drop missing rows
df_clean = df.dropna()

# Standardize
scaler = StandardScaler()
scaled = scaler.fit_transform(df_clean)

# PCA
pca = PCA()
pca.fit(scaled)

# Loadings
loadings = pd.DataFrame(
    pca.components_.T,
    columns=[f"PC{i+1}" for i in range(len(pca.components_))],
    index=usecols
)
loadings.to_csv("pca_loadings.csv", index=True)

# Variance explained
explained = pd.DataFrame({
    "PC": [f"PC{i+1}" for i in range(len(pca.explained_variance_ratio_))],
    "Explained Variance (%)": pca.explained_variance_ratio_ * 100
})
explained.to_csv("pca_explained_variance.csv", index=False)

# Scree plot
sns.set(style="whitegrid")
plt.figure(figsize=(8, 5))
sns.lineplot(
    x=range(1, len(pca.explained_variance_ratio_) + 1),
    y=pca.explained_variance_ratio_ * 100,
    marker="o"
)
plt.title("Scree Plot")
plt.xlabel("Principal Component")
plt.ylabel("Explained Variance (%)")
plt.xticks(range(1, len(pca.explained_variance_ratio_) + 1))
plt.tight_layout()
plt.savefig("scree_plot.png")

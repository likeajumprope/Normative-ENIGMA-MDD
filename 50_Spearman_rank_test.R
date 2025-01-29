# Install Bioconductor manager if not already installed
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# Install GO.db, a dependency for WGCNA
BiocManager::install("GO.db")

# Load required libraries
library(WGCNA)      # Weighted Gene Co-expression Network Analysis
library(ggplot2)    # Data visualization
library(ggrepel)    # Prevents label overlap in ggplot

# Load and preprocess the normative Cohen's d values
Cohen_Normative <- read.csv("./40_analysis/LMM_MDDvsHC_rawzscores2.csv")
Cohen_Normative <- Cohen_Normative[1:35, c(1,2)]  # Select first 35 rows and first 2 columns
colnames(Cohen_Normative)[c(1,2)] <- c("region_Norm", "Cohen_Norm")  # Rename columns

# Load and preprocess the Molecular Psychiatry Cohen's d values
Cohen_MolPsy = read.csv("Cohen_Lianne_MolPsy.csv", header = FALSE)
Cohen_MolPsy <- Cohen_MolPsy[1:35, c(5, 6)]  # Select first 35 rows and columns 5 and 6
colnames(Cohen_MolPsy)[c(1,2)] <- c("Cohen_Mol", "region_Mol")  # Rename columns
Cohen_MolPsy$Cohen_Mol <- Cohen_MolPsy$Cohen_Mol * (-1)  # Invert Cohen's d values

# Sort both datasets by region for accurate comparison
sort.MolPsy <- with(Cohen_MolPsy, Cohen_MolPsy[order(region_Mol), ])
sort.Normative <- with(Cohen_Normative, Cohen_Normative[order(region_Norm), ])

# Combine both datasets into one dataframe
df <- cbind(sort.MolPsy, sort.Normative)

# Base R scatter plot for quick visualization
plot(sort.MolPsy$Cohen_Mol, sort.Normative$Cohen_Norm, xlim = c(0, 0.15), ylim = c(0, 0.15))

# Publication-quality scatter plot using ggplot2
p <- ggplot(df, aes(x = Cohen_Norm, y = Cohen_Mol, label = region_Mol)) +
  geom_point(size = 3, color = "steelblue") +  # Customizable point size/color
  xlim(-0.1, 0.20) +  # X-axis limits
  ylim(-0.1, 0.20) +  # Y-axis limits
  # Uncomment below for label repulsion to avoid overlap
  # geom_text_repel(
  #   size = 4,                # Font size of labels
  #   box.padding = 0.5,       # Space around labels
  #   point.padding = 0.3,     # Space between point and label
  #   max.overlaps = 10        # Manage overlapping labels
  # ) +
  labs(
    x = "Cohen's d values, extracted from z-scores",
    y = "Cohen's d values, Schmaal et al., 2017"
  ) +
  theme_minimal(base_size = 14) +  # Clean, minimalist theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center title
    axis.title = element_text(face = "bold"),               # Bold axis titles
    panel.grid.major = element_line(color = "gray90")      # Light gridlines
  )

# Save the plot in high resolution for publication
ggsave("./images/scatter_plot_cohen.png", width = 6, height = 5, dpi = 300)

# Linear regression analysis
lm(df$Cohen_Mol ~ df$Cohen_Norm)

# Spearman rank correlation (non-parametric)
spearman <- cor.test(x = df$Cohen_Mol, y = df$Cohen_Norm, method = 'spearman')

# Pearson correlation (parametric)
pearson <- cor.test(x = df$Cohen_Mol, y = df$Cohen_Norm, method = 'pearson')

# Biweight midcorrelation (robust to outliers)
bicor <- bicor(df$Cohen_Mol, df$Cohen_Norm)
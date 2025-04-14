🏀 marchmadnessmodels
Predicting college basketball postseason matchups using a "similar games" methodology and machine learning.
Originally inspired an article written at Carnegie Mellon Sports Analytics Club (CMSAC) in 2018.

🧠 What This Is
This repo is built to model and predict college basketball postseason games – not just March Madness. We leverage box score data, player size, conference strength, game location, and more to compare games across seasons and find similar matchups from the past.

At the heart of this approach is a "similar games" methodology first developed at CMSAC. This lets us find historical comps to any matchup, then weight those comps using machine learning (XGBoost) to estimate win probabilities.


Repo structure:

marchmadnessmodels/
│
├── excel_files/            # Source spreadsheets (box scores, rosters, locations, etc.)
├── dataframe_generation/   # Turns Excel data into ready-to-train dataframes
├── ml_training/            # Trains ML models (XGBoost) to weight factors in matchups
├── comparison_files/       # Functions to search for similar historical games
├── results_tracker/        # Stores results + outcome evaluations

🔍 Methodology
Ingest + Clean Data: Scrapers pull raw data → cleaned & stored in Excel

Feature Generation: From Excel, generate features like:
 - Box score stats
 - Team minutes continuity
 - Player size metrics
 - Conference strength
 - Neutral/away/home context

Train Models:

XGBoost models trained on historic data

Outputs: feature weights → how important each stat is in driving outcomes

Find Similar Games:

Given a matchup, find historical games with similar team profiles, weight those games by similarity and ML-learned importance, to predict outcomes using those weighted comps.

# 📈 Example Use Cases
- Predict NCAA Tournament matchups
- Model NIT, CBI, or other postseason brackets
- Compare mid-major vs power conference dynamics historically
- Evaluate how style and roster construction travel across tournament formats


Built by someone who should probably be asleep right now




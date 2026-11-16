# Music recommender

Open the **Recommendations** tab in MusicBrainz explorer and type at least three characters to see matching artists, songs, and albums. Select a suggestion to get recommendations, or use **Find recommendations** to search the full catalog. Choose **Artist**, **Song**, or **Album** to narrow the search. Album recommendations start from the album's credited artists. Results link to their catalog records and show shared genres, recording tags, or credits when available.

## Run on this PC

From PowerShell:

~~~powershell
cd "<project folder>\music-recommender"
.\start.ps1 -WithOllama
~~~

The launcher uses the R 4.5.3 installation configured for this project and opens the app in your browser. The local Ollama model is Qwen3 4B. Try **Recommend songs like Teardrop by Massive Attack** in the natural-language search. Running `.\start.ps1` without the switch opens the same app for direct catalog search.

You can also launch from an R console:

~~~r
shiny::runApp("<project folder>", launch.browser = TRUE)
~~~

For natural-language search with this launch method, start Ollama separately with `ollama serve`. If the model was removed, restore it with `ollama pull qwen3:4b`. The app sends the request text only to the local Ollama server.

## Rebuild the recommendation model

Use R with DBI, DuckDB, Shiny, httr, and jsonlite installed. To retrain after refreshing the MusicBrainz database, stop the app and run:

~~~powershell
$r = Join-Path $env:LOCALAPPDATA 'Programs\R\R-4.5.3\bin\Rscript.exe'
& $r train.R ..\data\musicbrainz.duckdb model
~~~

Training reads the original DuckDB in read-only mode and writes `model/artists.duckdb`. Set `MUSIC_RECOMMENDER_MODEL` to use a different artifact path or `MUSICBRAINZ_DB` to point the app to a relocated copy of the original database.

The ranker fits genre weights from the catalog and learns which artist pairs resemble artists with shared credits. Song ranking also uses recording tags, year, duration and rating count where available. The training holdout tests the shared-credit proxy. This dataset has no listener history or audio, so the app recommends from catalog relationships rather than acoustic similarity or individual listening habits. Qwen3 interprets the request. It runs with its pretrained weights.

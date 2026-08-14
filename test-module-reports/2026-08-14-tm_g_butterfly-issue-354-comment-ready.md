## Manual testing notes: tm_g_butterfly (main vs redesign_extraction@main)

Pilot publish for the module-comparison workflow.

Source report:
- https://github.com/insightsengineering/teal.osprey/blob/redesign_extraction%40main/test-module-reports/2026-08-14-tm_g_butterfly-main-vs-redesign_extraction@main.md

Show R Code files:
- main: https://github.com/insightsengineering/teal.osprey/blob/redesign_extraction%40main/test-module-reports/2026-08-14-tm_g_butterfly-main-show-r-code.R
- redesign_extraction@main: https://github.com/insightsengineering/teal.osprey/blob/redesign_extraction%40main/test-module-reports/2026-08-14-tm_g_butterfly-redesign_extraction@main-show-r-code.R

Draft comment source:
- https://github.com/insightsengineering/teal.osprey/blob/redesign_extraction%40main/test-module-reports/2026-08-14-tm_g_butterfly-gh-comment.md

### Summary (from report)

- Module output (main vs redesign_extraction@main): Unable to verify automatically; screenshots captured for both branches and require manual visual comparison.
- Teal report preview (expanded cards): Different.
- Show R Code: Different.
- Should code paths yield same results: Yes (differences appear implementation-level, not endpoint intent).
- Final status: Passed.

### Screenshots

| Branch | Module output | Show R Code modal | Report preview |
|--------|---------------|-------------------|----------------|
| main | ![tm_g_butterfly main module output](https://raw.githubusercontent.com/insightsengineering/teal.osprey/redesign_extraction%40main/test-module-screenshots/tm_g_butterfly-main-module-output.png) | ![tm_g_butterfly main show r code](https://raw.githubusercontent.com/insightsengineering/teal.osprey/redesign_extraction%40main/test-module-screenshots/tm_g_butterfly-main-show-r-code.png) | ![tm_g_butterfly main report preview](https://raw.githubusercontent.com/insightsengineering/teal.osprey/redesign_extraction%40main/test-module-screenshots/tm_g_butterfly-main-report-show-report.png) |
| redesign_extraction@main | ![tm_g_butterfly redesign module output](https://raw.githubusercontent.com/insightsengineering/teal.osprey/redesign_extraction%40main/test-module-screenshots/tm_g_butterfly-redesign_extraction@main-module-output.png) | ![tm_g_butterfly redesign show r code](https://raw.githubusercontent.com/insightsengineering/teal.osprey/redesign_extraction%40main/test-module-screenshots/tm_g_butterfly-redesign_extraction@main-show-r-code.png) | ![tm_g_butterfly redesign report preview](https://raw.githubusercontent.com/insightsengineering/teal.osprey/redesign_extraction%40main/test-module-screenshots/tm_g_butterfly-redesign_extraction@main-report-show-report.png) |

### Detailed diff and full code

Please see the full prepared draft (includes unified diff and full Show R Code blocks):
- https://github.com/insightsengineering/teal.osprey/blob/redesign_extraction%40main/test-module-reports/2026-08-14-tm_g_butterfly-gh-comment.md

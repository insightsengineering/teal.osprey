# Fix for ggplot2 4.0 Warnings

## Issue
With ggplot2 4.0+, plotting modules generate warnings:
```
`height` was translated to `width`
```

## Root Cause
The warning originates from the `osprey` package functions:
- `g_ae_sub()` in `R/g_ae_sub.R`  
- `g_events_term_id()` in `R/g_events_term_id.R`

Both functions use `geom_errorbarh()` with the deprecated `height` parameter.

## Fix
In ggplot2 4.0+, for horizontal error bars (`geom_errorbarh`), the parameter controlling the width of error bar caps was renamed from `height` to `width` for consistency.

### Changes Made
1. **g_ae_sub.R (line 342)**: Changed `height = 0.3` to `width = 0.3`
2. **g_events_term_id.R (line 305)**: Changed `height = 0.4` to `width = 0.4`

### Affected Modules
- `tm_g_ae_sub` - Uses `osprey::g_ae_sub()`
- `tm_g_events_term_id` - Uses `osprey::g_events_term_id()`

## Patch File
A patch file `osprey-fix-ggplot2-warnings.patch` has been created that can be applied to the osprey package to fix this issue.

## Next Steps
The fix needs to be applied to the `osprey` package repository (https://github.com/insightsengineering/osprey).
Once the fix is merged and released in osprey, teal.osprey will automatically benefit from it due to the dependency specification in DESCRIPTION.

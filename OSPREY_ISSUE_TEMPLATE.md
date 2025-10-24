# Issue for osprey Repository

## Title
[Bug]: ggplot2 4.0+ warnings: `height` was translated to `width` in geom_errorbarh

## Description
With ggplot2 4.0+, the `g_ae_sub` and `g_events_term_id` functions generate warnings when plotting:

```
`height` was translated to `width`
```

This warning is caused by using the deprecated `height` parameter in `geom_errorbarh()` calls. In ggplot2 4.0+, this parameter was renamed to `width` for consistency (since for horizontal error bars, the parameter controls the width of the error bar caps).

## Affected Files
- `R/g_ae_sub.R` (line 342)
- `R/g_events_term_id.R` (line 305)

## Fix
Change `height` parameter to `width` in both `geom_errorbarh()` calls:

### g_ae_sub.R
```r
# Current (line 340-343):
geom_errorbarh(
  aes(xmin = .data$lower, xmax = .data$upper, y = .data$level),
  height = 0.3
) +

# Should be:
geom_errorbarh(
  aes(xmin = .data$lower, xmax = .data$upper, y = .data$level),
  width = 0.3
) +
```

### g_events_term_id.R
```r
# Current (line 305):
geom_errorbarh(mapping = aes(xmax = .data$upper_ci, xmin = .data$lower_ci, y = term), height = 0.4) +

# Should be:
geom_errorbarh(mapping = aes(xmax = .data$upper_ci, xmin = .data$lower_ci, y = term), width = 0.4) +
```

## Impact
This fix will:
- Eliminate ggplot2 4.0+ deprecation warnings
- Maintain backward compatibility (the visual output remains the same)
- Ensure compatibility with future ggplot2 versions

## Testing
The change can be tested by running any examples that use these functions with ggplot2 4.0+ and verifying:
1. No warnings are generated
2. The plots render identically to before

## Patch
A patch file is available at: `osprey-fix-ggplot2-warnings.patch`

## Related Issue
Filed in teal.osprey: [Bug]: Warnings when plotting

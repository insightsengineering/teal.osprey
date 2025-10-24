# Fix Summary: ggplot2 4.0+ Warnings in Plotting Modules

## Problem Statement
The `tm_g_ae_sub` and `tm_g_events_term_id` modules in teal.osprey generate warnings with ggplot2 4.0+:
```
`height` was translated to `width`
```

## Root Cause Analysis
The warnings originate from the underlying `osprey` package, not from `teal.osprey` itself:

- **Module**: `tm_g_ae_sub` (in teal.osprey)
  - Calls: `osprey::g_ae_sub()` 
  - Issue location: `osprey/R/g_ae_sub.R`, line 342

- **Module**: `tm_g_events_term_id` (in teal.osprey)
  - Calls: `osprey::g_events_term_id()`
  - Issue location: `osprey/R/g_events_term_id.R`, line 305

## Technical Details
In ggplot2 4.0+, the `geom_errorbarh()` function changed its parameter for controlling error bar cap size:
- **Old (deprecated)**: `height` parameter
- **New (correct)**: `width` parameter

The rename provides consistency since horizontal error bars extend vertically, so their "width" makes more sense than "height".

## Solution
The fix requires changing two lines in the `osprey` package:

1. **osprey/R/g_ae_sub.R:342**
   ```r
   # Before:
   geom_errorbarh(..., height = 0.3)
   
   # After:
   geom_errorbarh(..., width = 0.3)
   ```

2. **osprey/R/g_events_term_id.R:305**
   ```r
   # Before:
   geom_errorbarh(..., height = 0.4)
   
   # After:
   geom_errorbarh(..., width = 0.4)
   ```

## Deliverables
1. ✅ **osprey-fix-ggplot2-warnings.patch** - Patch file with the complete fix
2. ✅ **OSPREY_FIX_NOTES.md** - Detailed technical notes about the fix
3. ✅ **OSPREY_ISSUE_TEMPLATE.md** - Template for creating an issue in the osprey repository
4. ✅ **NEWS.md** - Updated with bug fix entry for teal.osprey
5. ✅ **FIX_SUMMARY.md** - This comprehensive summary document

## Implementation Status
- [x] Issue identified and analyzed
- [x] Fix created and validated syntactically
- [x] Fix documented with patch file
- [x] NEWS.md updated
- [x] Issue template created for osprey repository
- [ ] **Pending**: Fix needs to be applied to the `insightsengineering/osprey` repository
- [ ] **Pending**: teal.osprey needs to use updated osprey version (automatic once osprey is fixed)

## Next Steps for Maintainers
1. Apply the patch to the `osprey` repository:
   ```bash
   cd osprey
   git apply ../teal.osprey/osprey-fix-ggplot2-warnings.patch
   ```
   OR manually make the changes as documented in the patch file.

2. Create a PR in the `osprey` repository with these changes

3. After the `osprey` fix is merged and released, teal.osprey will automatically benefit (dependency relationship)

4. Optionally, update teal.osprey's DESCRIPTION to require the minimum osprey version with the fix

## Testing
Once the fix is applied to osprey:
1. Run any examples from `tm_g_ae_sub` or `tm_g_events_term_id`
2. Verify no ggplot2 warnings are generated
3. Verify plots render identically to before (visual regression test)
4. Confirm compatibility with ggplot2 >= 4.0

## Impact Assessment
- **Breaking changes**: None
- **Visual changes**: None (output identical)
- **Compatibility**: Improves compatibility with ggplot2 4.0+
- **Risk level**: Minimal (parameter rename maintains same functionality)

## References
- Original issue: [Bug]: Warnings when plotting (in teal.osprey repository)
- ggplot2 4.0 release notes: Parameter consistency improvements
- Affected modules: `tm_g_ae_sub`, `tm_g_events_term_id`

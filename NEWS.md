# roadDB 0.2.0

## New Features
- Add `road_get_sql()` to query SQL strings directly.
- Add `comment` column to `road_get_dates()` following a user-request.

## Bug Fixes
- Replace out-of-bounds standad deviation values (e.g., "0", "1000000") with `NA` in `road_get_dates()`.
- Fix irregular spaces in `road_list_argument_values("tool_list")` causing errors.

## Internal Improvements
- Replace `T` and `F` with `TRUE` and `FALSE` throughout the codebase.
- Improve error handling with informative messages when database connection fails.
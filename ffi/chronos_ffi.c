/*
 * Chronos FFI
 * Wall clock time bindings using POSIX time functions
 */

#include <lean/lean.h>
#include <time.h>
#include <stdint.h>

/* ============================================================================
 * Helper: Create IO error result
 * ============================================================================ */

static lean_obj_res mk_io_error(const char* msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

/* ============================================================================
 * Helper: Create a Prod (pair)
 * Prod A B is represented as: lean_alloc_ctor(0, 2, 0) with fields set
 * ============================================================================ */

static lean_obj_res mk_pair(lean_obj_arg fst, lean_obj_arg snd) {
    lean_object* pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, fst);
    lean_ctor_set(pair, 1, snd);
    return pair;
}

/* ============================================================================
 * chronos_now : IO (Int64 × UInt32)
 *
 * Get current wall clock time as (seconds, nanoseconds) since Unix epoch.
 * ============================================================================ */

LEAN_EXPORT lean_obj_res chronos_now(lean_obj_arg world) {
    struct timespec ts;

    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
        return mk_io_error("clock_gettime failed");
    }

    /* Return pair of (seconds : Int64, nanoseconds : UInt32) */
    lean_obj_res seconds = lean_int64_to_int(ts.tv_sec);
    lean_obj_res nanos = lean_box_uint32((uint32_t)ts.tv_nsec);

    return lean_io_result_mk_ok(mk_pair(seconds, nanos));
}

/* ============================================================================
 * DateTime representation
 *
 * We return DateTime fields as a nested tuple:
 * (year : Int32, month : UInt8, day : UInt8, hour : UInt8,
 *  minute : UInt8, second : UInt8, nanosecond : UInt32)
 *
 * Represented as: Prod Int32 (Prod UInt8 (Prod UInt8 (Prod UInt8 (Prod UInt8 (Prod UInt8 UInt32)))))
 * ============================================================================ */

static lean_obj_res mk_datetime_tuple(int32_t year, uint8_t month, uint8_t day,
                                       uint8_t hour, uint8_t minute, uint8_t second,
                                       uint32_t nanosecond) {
    /* Build from inside out:
     * innermost: Prod UInt8 UInt32 = (second, nanosecond)
     * then: Prod UInt8 (Prod UInt8 UInt32) = (minute, ...)
     * etc.
     *
     * Note: We use lean_int_to_int32 which creates a boxed Int representation
     * that Lean will interpret as Int32.
     */
    lean_obj_res p6 = mk_pair(lean_box(second), lean_box_uint32(nanosecond));
    lean_obj_res p5 = mk_pair(lean_box(minute), p6);
    lean_obj_res p4 = mk_pair(lean_box(hour), p5);
    lean_obj_res p3 = mk_pair(lean_box(day), p4);
    lean_obj_res p2 = mk_pair(lean_box(month), p3);
    /* For Int32, box it directly since it fits in a small int */
    lean_obj_res p1 = mk_pair(lean_box((size_t)(int64_t)year), p2);

    return p1;
}

/* ============================================================================
 * chronos_to_utc : Int64 → UInt32 → IO DateTimeTuple
 *
 * Convert Unix timestamp to UTC date/time components.
 * ============================================================================ */

LEAN_EXPORT lean_obj_res chronos_to_utc(lean_obj_arg seconds_obj, uint32_t nanos, lean_obj_arg world) {
    int64_t seconds = lean_int64_of_int(seconds_obj);
    lean_dec(seconds_obj);

    time_t t = (time_t)seconds;
    struct tm tm_result;

    if (gmtime_r(&t, &tm_result) == NULL) {
        return mk_io_error("gmtime_r failed");
    }

    lean_obj_res tuple = mk_datetime_tuple(
        (int32_t)(tm_result.tm_year + 1900),  /* year */
        (uint8_t)(tm_result.tm_mon + 1),       /* month: 1-12 */
        (uint8_t)tm_result.tm_mday,            /* day: 1-31 */
        (uint8_t)tm_result.tm_hour,            /* hour: 0-23 */
        (uint8_t)tm_result.tm_min,             /* minute: 0-59 */
        (uint8_t)tm_result.tm_sec,             /* second: 0-59 */
        nanos                                   /* nanosecond */
    );

    return lean_io_result_mk_ok(tuple);
}

/* ============================================================================
 * chronos_to_local : Int64 → UInt32 → IO DateTimeTuple
 *
 * Convert Unix timestamp to local date/time components.
 * ============================================================================ */

LEAN_EXPORT lean_obj_res chronos_to_local(lean_obj_arg seconds_obj, uint32_t nanos, lean_obj_arg world) {
    int64_t seconds = lean_int64_of_int(seconds_obj);
    lean_dec(seconds_obj);

    time_t t = (time_t)seconds;
    struct tm tm_result;

    if (localtime_r(&t, &tm_result) == NULL) {
        return mk_io_error("localtime_r failed");
    }

    lean_obj_res tuple = mk_datetime_tuple(
        (int32_t)(tm_result.tm_year + 1900),  /* year */
        (uint8_t)(tm_result.tm_mon + 1),       /* month: 1-12 */
        (uint8_t)tm_result.tm_mday,            /* day: 1-31 */
        (uint8_t)tm_result.tm_hour,            /* hour: 0-23 */
        (uint8_t)tm_result.tm_min,             /* minute: 0-59 */
        (uint8_t)tm_result.tm_sec,             /* second: 0-59 */
        nanos                                   /* nanosecond */
    );

    return lean_io_result_mk_ok(tuple);
}

/* ============================================================================
 * chronos_from_utc : Int32 → UInt8 → UInt8 → UInt8 → UInt8 → UInt8 → UInt32 → IO (Int64 × UInt32)
 *
 * Convert UTC date/time components back to Unix timestamp.
 * ============================================================================ */

LEAN_EXPORT lean_obj_res chronos_from_utc(
    int32_t year, uint8_t month, uint8_t day,
    uint8_t hour, uint8_t minute, uint8_t second,
    uint32_t nanosecond,
    lean_obj_arg world
) {
    struct tm tm_input;
    tm_input.tm_year = year - 1900;
    tm_input.tm_mon = month - 1;
    tm_input.tm_mday = day;
    tm_input.tm_hour = hour;
    tm_input.tm_min = minute;
    tm_input.tm_sec = second;
    tm_input.tm_isdst = 0;  /* UTC has no DST */

    /* timegm is a BSD/GNU extension that converts struct tm in UTC to time_t
     * On systems without timegm, we could use a portable workaround */
    time_t t = timegm(&tm_input);

    if (t == (time_t)-1) {
        /* Note: -1 could be a valid timestamp (1969-12-31 23:59:59 UTC)
         * but we'll treat it as an error for simplicity */
        return mk_io_error("timegm failed");
    }

    lean_obj_res seconds = lean_int64_to_int((int64_t)t);
    lean_obj_res nanos = lean_box_uint32(nanosecond);

    return lean_io_result_mk_ok(mk_pair(seconds, nanos));
}

/* ============================================================================
 * chronos_get_timezone_offset : IO Int32
 *
 * Get the current timezone offset in seconds (local - UTC).
 * Positive for east of UTC, negative for west.
 * ============================================================================ */

LEAN_EXPORT lean_obj_res chronos_get_timezone_offset(lean_obj_arg world) {
    time_t now = time(NULL);
    struct tm local_tm, utc_tm;

    if (localtime_r(&now, &local_tm) == NULL) {
        return mk_io_error("localtime_r failed");
    }

    if (gmtime_r(&now, &utc_tm) == NULL) {
        return mk_io_error("gmtime_r failed");
    }

    /* To find the offset, interpret both tm structs as local time using mktime.
     * mktime(&local_tm) gives the correct epoch seconds.
     * mktime(&utc_tm) treats UTC fields as if they were local time, which is wrong
     * by exactly the timezone offset. So the difference is the offset. */
    time_t local_as_local = mktime(&local_tm);
    time_t utc_as_local = mktime(&utc_tm);

    int32_t offset = (int32_t)(local_as_local - utc_as_local);

    /* Box as small int - offset fits within range */
    return lean_io_result_mk_ok(lean_box((size_t)(int64_t)offset));
}

/*****************************************************************************
  Advent of code - Day 22
 *****************************************************************************/

DROP TABLE IF EXISTS "InputData";
CREATE TABLE "InputData" (
    val INT
);

\copy "InputData"(val) FROM '/path/to/input.txt' WITH (FORMAT text);

CREATE OR REPLACE FUNCTION gen_new(n INT)
  RETURNS INT
  LANGUAGE plpgsql
AS $$
DECLARE
    tmp BIGINT;  -- Temporary variable in BIGINT to avoid overflow
BEGIN
    tmp := n;

    -- tmp ^= tmp * 64
    tmp := tmp # (tmp * 64);
    tmp := tmp % 16777216;

    -- tmp ^= tmp // 32
    tmp := tmp # (tmp / 32);
    tmp := tmp % 16777216;

    -- tmp ^= tmp * 2048
    tmp := tmp # (tmp * 2048);
    tmp := tmp % 16777216;

    -- Now tmp < 16777216, so safe to cast back to int
    RETURN tmp::INT;
END;
$$;

/******************************************************************************
Part 1
            - 2000 iterations of gen_new on each row
            - Summation at the end
******************************************************************************/
CREATE OR REPLACE PROCEDURE part_1()
AS $$
DECLARE
    i     INT;
    total BIGINT;
BEGIN
    DROP TABLE IF EXISTS temp_data;
    CREATE TEMP TABLE temp_data AS
        SELECT val FROM "InputData";

    FOR i IN 1..2000 LOOP
        UPDATE temp_data
           SET val = gen_new(val);
    END LOOP;

    SELECT SUM(val) INTO total FROM temp_data;

    RAISE NOTICE 'Result of part_1 (sum): %', total;
END;
$$;

/******************************************************************************
 Part 2
           - For each val in "InputData":
             * get 2000 deltas => (delta_diff, delta_nb)
             * For each 4-sequence of deltas (idx=0..1996):
               - pattern = (d1, d2, d3, d4)
               - patterns_roi[pattern] += nb_of_4th_element
                 (only once per init)
           - Finally, print max pattern sum.
******************************************************************************/
CREATE OR REPLACE PROCEDURE part_2()
LANGUAGE plpgsql
AS $$
DECLARE
    rec     RECORD;
    idx     INT;
    d1      INT;
    d2      INT;
    d3      INT;
    d4      INT;
    nb      INT;
    max_val BIGINT;
BEGIN
    /*
      We'll create:
        1) TEMP TABLE patterns_roi => accumulates (diff1, diff2, diff3, diff4, sum_nb).
        2) For each val in "InputData":
           - TEMP TABLE deltas => store "seq", "delta_diff", "delta_nb".
           - TEMP TABLE added => track patterns used once for this val.
    */
    DROP TABLE IF EXISTS patterns_roi;
    CREATE TEMP TABLE patterns_roi (
        diff1  INT,
        diff2  INT,
        diff3  INT,
        diff4  INT,
        sum_nb BIGINT
    ) ON COMMIT DROP;

    -- Cursor-like loop over "InputData"
    FOR rec IN SELECT val FROM "InputData" LOOP
        ----------------------------------------------------------------------
        -- 1) Build "deltas" for the current record
        ----------------------------------------------------------------------
        DROP TABLE IF EXISTS deltas;
        CREATE TEMP TABLE deltas (
            seq INT,
            delta_diff INT,
            delta_nb   INT
        ) ON COMMIT DROP;

        DECLARE
            i INT := 0;
            b INT;
            new_n INT := rec.val;
        BEGIN
            WHILE i < 2000 LOOP
                b := new_n % 10;
                new_n := gen_new(new_n);
                nb := new_n % 10;

                INSERT INTO deltas(seq, delta_diff, delta_nb)
                VALUES (i, nb - b, nb);

                i := i + 1;
            END LOOP;
        END;

        ----------------------------------------------------------------------
        -- 2) TEMP TABLE "added" tracks whether a 4-diff pattern is used once
        ----------------------------------------------------------------------
        DROP TABLE IF EXISTS added;
        CREATE TEMP TABLE added (
            diff1 INT,
            diff2 INT,
            diff3 INT,
            diff4 INT,
            UNIQUE(diff1, diff2, diff3, diff4)
        ) ON COMMIT DROP;

        ----------------------------------------------------------------------
        -- 3) Slide from idx=0..1996
        ----------------------------------------------------------------------
        FOR idx IN 0..1996 LOOP

            SELECT delta_diff
              INTO d1
              FROM deltas
             WHERE seq = idx;

            SELECT delta_diff
              INTO d2
              FROM deltas
             WHERE seq = idx + 1;

            SELECT delta_diff
              INTO d3
              FROM deltas
             WHERE seq = idx + 2;

            SELECT delta_diff, delta_nb
              INTO d4, nb
              FROM deltas
             WHERE seq = idx + 3;

            -- Only insert if pattern wasn't used for this init
            BEGIN
                INSERT INTO added(diff1, diff2, diff3, diff4)
                VALUES (d1, d2, d3, d4);
            EXCEPTION
                WHEN unique_violation THEN
                    -- Already added for this init, so skip
                    CONTINUE;
            END;

            -- Insert or update in patterns_roi
            IF EXISTS (
                SELECT 1
                  FROM patterns_roi
                 WHERE diff1 = d1 AND diff2 = d2
                   AND diff3 = d3 AND diff4 = d4
            ) THEN
                UPDATE patterns_roi
                   SET sum_nb = sum_nb + nb
                 WHERE diff1 = d1 AND diff2 = d2
                   AND diff3 = d3 AND diff4 = d4;
            ELSE
                INSERT INTO patterns_roi(diff1, diff2, diff3, diff4, sum_nb)
                VALUES (d1, d2, d3, d4, nb);
            END IF;

        END LOOP;

    END LOOP;

    SELECT MAX(sum_nb) INTO max_val
    FROM patterns_roi;

    RAISE NOTICE 'Result of part_2 (max): %', max_val;
END;
$$;

CALL part_1();
CALL part_2();

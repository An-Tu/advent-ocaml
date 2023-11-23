let test_find_marker expected_position str () =
  Alcotest.(check int)
    "same int" expected_position
    (Lib.Helpers.find_marker str)

let () =
  let open Alcotest in
  run "Libs tests"
    [
      ( "find marker",
        [
          test_case "test 1" `Quick
            (test_find_marker 7 "mjqjpqmgbljsphdztnvjfqwrcgsmlb");
          test_case "test 2" `Quick
            (test_find_marker 5 "bvwbjplbgvbhsrlpgdmjqwftvncz");
          test_case "test 3" `Quick
            (test_find_marker 6 "nppdvjthqldpwncqszvftbrmjlhg");
          test_case "test 4" `Quick
            (test_find_marker 10 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg");
          test_case "test 5" `Quick
            (test_find_marker 11 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw");
        ] );
    ]

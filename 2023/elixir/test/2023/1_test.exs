import AOC

aoc_test 2023, 1, async: true do
  describe "p1" do
    test "works for the example input" do
      assert p1(example_string()) == 142
    end
  end
end

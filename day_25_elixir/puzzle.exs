defmodule Solution do
  @max_height 5
  @width 5

  def solve(path \\ "input.txt") do
    File.read!(path)
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.split(&1, "\n"))
    |> Enum.map(fn
      ["....." | rest] ->
        for {row, current_size} <- Enum.with_index(rest),
            {"#", index} <- row |> String.split("") |> Enum.with_index(),
            reduce: {:key, %{}} do
          {:key, sizes} -> {:key, Map.put_new(sizes, index, @width - current_size)}
        end

      ["#####" | rest] ->
        for {row, current_size} <- Enum.with_index(rest),
            {".", index} <- row |> String.split("") |> Enum.with_index(),
            reduce: {:lock, %{}} do
          {:lock, sizes} -> {:lock, Map.put_new(sizes, index, current_size)}
        end
    end)
    |> then(fn sizes ->
      for {:key, key_size} <- sizes, {:lock, lock_size} <- sizes, reduce: 0 do
        sum ->
          Enum.zip_reduce(key_size, lock_size, :ok, fn
            {k, x}, {k, y}, :ok -> if x + y <= @max_height, do: :ok, else: :error
            _, _, :error -> :error
          end)
          |> case do
            :ok -> sum + 1
            :error -> sum
          end
      end
    end)
  end
end

IO.inspect(Solution.solve())

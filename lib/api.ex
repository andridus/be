defmodule Be.Api do
  @moduledoc false
  defmacro __using__(_) do
    quote do
      import Ecto.Query
      import Be.Api

      def schema, do: @schema

      def changeset(changeset, params, type \\ "create") do
        apply(schema(), :"changeset_#{type}", [changeset, params])
      end

      def json_fields(append) do
        schema().__json__() ++ append
      end

      def get_by(params \\ [where: [], order: [asc: :inserted_at]]) do
        get_by!(params)
        |> case do
          nil -> {:error, :not_found}
          data -> {:ok, data}
        end
      end

      def get_by!(params \\ [where: [], order: [asc: :inserted_at]]) do
        params
        |> default_params()
        |> repo().one()
      end

      def get(_id, _params \\ [where: [], order: [asc: :inserted_at]])
      def get(nil, _params), do: {:error, :id_is_nil}
      def get(id, params ) do
        get!(id, params)
        |> case do
          nil -> {:error, :not_found}
          data -> {:ok, data}
        end
      end

      def get!(id, params \\ [where: [], order: [asc: :inserted_at]]) do
        params
        |> default_params()
        |> repo().get(id)
      end

      def blank(), do: struct(schema())

      def all(params \\ [where: [], order: []]) do
        params
        |> default_params()
        |> repo().all()
      end

      def insert(%Ecto.Changeset{} = model),
        do: model |> repo().insert()
      def insert(params) do
        schema()
        |> struct()
        |> schema().changeset_insert(params)
        |> repo().insert()
      end
      def exists?(params) do
        params
        |> default_params()
        |> repo().exists?()
      end

      def update(%Ecto.Changeset{} = model),
        do: model |> repo().update()
      def update(%{"id" => id} = model) do
        params = Map.drop(model, ["id"])
        __update_model__(id, params)
      end
      def update(%{id: id} = model) do
        params = Map.drop(model, [:id])
        __update_model__(id, params)
      end
      def update(id, params) when is_bitstring(id), do: __update_model__(id, params)
      def update(%{id: id}, params) when is_bitstring(id), do: __update_model__(id, params)
      def update(_invalid_model, _params), do: {:error, :invalid_model}

      defp __update_model__(id, params) do
        id
        |> get!()
        |> schema().changeset_update(params)
        |> repo().update()
      end

      def delete(id) when is_bitstring(id),
        do: id |> get!() |> delete()
      def delete(model), do: model |> repo().delete()

      def default_params(params, sc \\ nil) do
        sch = from(sc || schema())
        params
        |> Enum.reduce(sch, fn
          {:where, params}, sch ->
            sch |> where(^default_conditions(params))
          {:or_where, params}, sch ->
            sch |> or_where(^default_conditions(params))
          {:order, params}, sch ->
            sch |> order_by(^params)
          {:preload, params}, sch ->
            lst = Enum.reduce(params, [], fn
              {k, v}, acc ->
                {_k, _t, opts} = schema().__live_fields__() |> List.keyfind!(k,0)
                [{k, default_params(v, opts[:schema])} | acc]
              k, acc when is_atom(k) ->
                [k | acc]
            end) |> Enum.reverse()
            sch |> preload(^lst)

          {:select, params}, sch ->
            sch |> select([c], map(c, ^params))

          {:limit, params}, sch ->
            sch |> limit(^params)

          {:offset, params}, sch ->
            sch |> offset(^params)

          {:inspect, true}, sch ->
              IO.inspect(sch)
          _, sch ->
            sch
        end)
      end

      defp default_conditions(params) do
        Enum.reduce(params, nil, fn
          {{:ilike, key}, value}, conditions ->
            value = "%#{value}%"
            if is_nil(conditions) do
              dynamic([p], ilike(field(p, ^key), ^value))
            else
              dynamic([p], ilike(field(p, ^key), ^value) and ^conditions)
            end
          {{:in, key}, value}, conditions ->
            if is_nil(conditions) do
              dynamic([p], field(p, ^key) in ^value)
            else
              dynamic([p], field(p, ^key) in ^value and ^conditions)
            end
          {:not_nil, key}, conditions ->
            if is_nil(conditions) do
              dynamic([p], not is_nil(field(p, ^key)))
            else
              dynamic([p], not is_nil(field(p, ^key)) and ^conditions)
            end
          {key, nil}, conditions ->
            if is_nil(conditions) do
              dynamic([p], is_nil(field(p, ^key)))
            else
              dynamic([p], is_nil(field(p, ^key)) and ^conditions)
            end

          {key, value}, conditions ->
            if is_nil(conditions) do
              dynamic([p], field(p, ^key) == ^value)
            else
              dynamic([p], field(p, ^key) == ^value and ^conditions)
            end
        end)
      end

      def count(params \\ []) do
        params
        |> default_params()
        |> repo().aggregate(:count, :id)
      end

      @doc """
        get json data
      """
      def json(_model, _include \\ [])
      def json(nil, _), do: nil
      def json(model, include) do
        model
        |> preload_json(include)
        |> Map.take(json_fields(include))
      end

      def insert_or_update(%Ecto.Changeset{action: :insert} = model), do: insert(model)
      def insert_or_update(%Ecto.Changeset{action: :update, data: %{id: _id}} = model), do: update(model)
      def insert_or_update(%{id: id} = model) when not is_nil(id), do: update(model)
      def insert_or_update(model), do: insert(model)

      defoverridable [changeset: 2, changeset: 3, json_fields: 1, get: 1, get: 2, get_by: 1, all: 1, insert: 1, update: 1, delete: 1, json: 2, count: 1]
    end
  end

  def repo, do: Application.get_env(:be, :repo)

  def preload_json(model, include \\ []) do
    model
    |> repo().preload(include)
    |> Map.take(include)
    |> Map.to_list()
    |> Enum.map(fn {key, value} ->
      module = get_module(model, key, include)
      if is_list(value) do
        {key, Enum.map(value, &(apply(module, :json, [&1]) |> unwrap()))}
      else
        {key, apply(module, :json, [value]) |> unwrap()}
      end
    end)
    |> Map.new()
    |> then(& Map.merge(model, &1))
  end

  def get_module(model, key, _includes) do
    Ecto.build_assoc(model, key, %{})
    |> Map.get(:__struct__)
    |> to_string()
    |> String.replace("Schema", "Api")
    |> atomize()
  end

  def atomize(string) when is_bitstring(string) do
    string |> String.to_existing_atom()
  rescue
    ArgumentError -> String.to_atom(string)
  end

  def unwrap({:ok, value}), do: value
  def unwrap(err), do: err
  def unwrap!({_, value}), do: value
end

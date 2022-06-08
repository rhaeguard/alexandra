# alexandra

It's Alexandra because it sounds similar to Cassandra?

Very very very simple db.

Supported operations

Create entity:

```
define Person
    id int
    name string
    last_name string
    date_of_birth date
end
```

Add data

```
add Person(1, "John", "Doe", "1997-01-12")
```

Read data

```
get Person                                  # gets all Persons
get Person where id > 10                    # gets all Persons where id > 10
get Person where id > 10 or name = 'Jane'  # gets all Janes whose ids are bigger than 10
```

Update data

```
modify Person to Person(_, _, 'Smith', _) where last_name = 'Smithe' 
```

Delete data

```
delete Person(1, _, _, _)
delete Person where id = 10
```
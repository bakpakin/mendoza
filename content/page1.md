{
    :title "Page 1"
    :template "main.html"
}

---

# Page 1

This is page 1

* hello
* world

* bork
* bark

::partial
* hello
* world

::partial[[

One paragraph

Two paragraph

* list
* more

::partial
{:title "title set in content"}
stuff

]]

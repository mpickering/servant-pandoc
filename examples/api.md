On proper introductions.
========================

Hello there.

As documentation is usually written for humans, it's often useful to introduce concepts with a few words.

This title is below the last
============================

You'll also note that multiple intros are possible.

POST `/greet`
=============

Request Body
------------

``` {.javascript}
"Hello, haskeller!"
```

Response
--------

-   Status code 201
-   If you use ?capital=true (application/json)
    ``` {.javascript}
    "HELLO, HASKELLER"
    ```

-   If you use ?capital=false (application/json)
    ``` {.javascript}
    "Hello, haskeller"
    ```

GET `/hello;lang=<value>/:name`
===============================

Captures
--------

-   *name*: name of the person to greet

GET Parameters
--------------

-   capital

    **Values**
    :   *true, false*

    **Description**
    :   Get the greeting message in uppercase (true) or not (false).Default is false.

Response
--------

-   Status code 200
-   If you use ?capital=true (application/json)
    ``` {.javascript}
    "HELLO, HASKELLER"
    ```

-   If you use ?capital=true (text/plain;charset=utf-8)
    ``` {.text}
    "HELLO, HASKELLER"
    ```

-   If you use ?capital=false (application/json)
    ``` {.javascript}
    "Hello, haskeller"
    ```

-   If you use ?capital=false (text/plain;charset=utf-8)
    ``` {.text}
    "Hello, haskeller"
    ```

DELETE `/greet/:greetid`
========================

Captures
--------

-   *greetid*: identifier of the greet msg to remove

<!-- -->

-   This endpoint is sensitive to the value of the **unicorns** HTTP
    header.

Response
--------

-   Status code 200
-   No response body


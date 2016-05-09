

# Module logi_fluentd_layout_json #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A logi_layout implementation which formats log messages as JSON string.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new(logi_layout_io_lib_format:new())</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new layout instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Equivalent to [`new(logi_layout_io_lib_format:new())`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(BaseLayout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Creates a new layout instance


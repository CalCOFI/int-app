# ivy

Sandbox for CalCOFI environmental data storytelling with intern Ivy.

## Notebooks

These web pages (\*.html) are typically rendered from Quarto (\*.qmd) source files:

<!-- Jekyll rendering -->
{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}

## Source

See the source files here:

- [github.com/CalCOFI/ivy](https://github.com/CalCOFI/ivy)


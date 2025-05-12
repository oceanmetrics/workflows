## html

These web pages (\*.html) are typically rendered from Quarto markdown (\*.qmd):

<!-- Jekyll rendering -->
{% for file in site.static_files %}
  {% if file.extname == '.html' or file.extname == '.txt' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }}) ({{ file.modified_time | date: "%Y-%m-%d %H:%M:%S" }}) 
  {% endif %}
{% endfor %}

## source

<!-- [Using site.github](https://jekyll.github.io/github-metadata/site.github/) -->
For more, including the source Quarto (*qmd) files and repository's README, see the Github repository 
<a href = "{{ site.github.repository_url }}">{{ site.github.owner_name }}/{{ site.github.repository_name }}</a>.

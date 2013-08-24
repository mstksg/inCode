require 'fileutils'

preferred_syntax = :sass
http_path = '/'
css_dir = '/tmp/static/css'
sass_dir = '/scss'
images_dir = '/static/img'
javascripts_dir = '/static/js'
relative_assets = true
fonts_dir = '/static/font'


if environment == :production
  output_style = :compressed
elsif environment == :development
  output_style = :expanded
end

# on_stylesheet_saved do |file|
#   if File.exists?(file)
#     filename = File.basename(file, File.extname(file))
#     File.cp(file, File.dirname(file) + "/" + filename + ".min" + File.extname(file))
#   end
# end

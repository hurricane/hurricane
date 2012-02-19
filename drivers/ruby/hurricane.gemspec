Gem::Specification.new do |s|
  s.name        = 'hurricane'
  s.version     = '1.0.0'
  s.date        = '2012-02-18'
  s.summary     = 'Hurricane driver'
  s.description = <<-desc
    The Hurricane package includes libraries to communicate with
    the Hurricane messaging system, encode/decode all Erlang terms,
    and provides a WSGI server for use with Hurricane
  desc
  s.authors     = ['Ilia Cheishvili']
  s.email       = 'ilia.cheishvili@gmail.com'
  s.files       = [
    'lib/hurricane.rb',
    'lib/erl_codec.rb',
    'lib/rack/handler/hurricane.rb'
  ]
  s.homepage    = 'http://icheishvili.github.com/hurricane'
end

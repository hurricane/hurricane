#!/usr/bin/env ruby

require 'uri'
require 'stringio'
require 'hurricane'

# Hurricane Rack Handler
module Rack
  module Handler
    class Hurricane

      def self.run(app, options={})
        options[:HurricaneType] ||= 'tcp'
        options[:HurricaneHost] ||= '127.0.0.1'
        options[:HurricanePort] ||= 3000
        options[:HurricaneProcessGroup] ||= 'http_handler'

        if options[:HurricaneType].eql?('tcp')
          gateway = ::Hurricane::Gateway.new(
            Erlang::SocketWrapper.new(
              options[:HurricaneHost], options[:HurricanePort]
            )
          )
          gateway.register_server(options[:HurricaneProcessGroup])
        else
          gateway = ::Hurricane::Gateway.new()
          gateway.send_ready_signal()
        end

        puts "Hurricane WSGI Server started " \
             "(Hurricane @ #{options[:HurricaneHost]}" \
             ":#{options[:HurricanePort]})"

        loop do
          httpreq = gateway.do_recv()
          rackenv = self.to_rack_hash(httpreq.data)
          rackenv['hurricane.gateway'] = gateway

          httpresp = ::Hurricane::Message.new()
          httpresp.type = 'response'
          httpresp.destination = httpreq.destination
          httpresp.tag = httpreq.tag

          rackenv.default = ''
          if rackenv['HTTP_EXPECT'].downcase() == '100-continue'
            httpresp.data = Erlang::Tuple.new(['100 Continue', [], None])
            gateway.do_send(httpresp)
            next
          end
          rackenv.default = nil

          code, headers, data = app.call(rackenv)
          body = StringIO.new()
          data.each do |chunk|
            body << chunk
          end
          httpresp.data = Erlang::Tuple.new(
            [code, headers, Erlang::Binary.new(body.string())]
          )
          body.close()
          if data.respond_to?(:close)
            data.close()
          end

          gateway.do_send(httpresp)
        end
      end

      # Additional server options (mostly Hurricane-related stuff)
      def self.valid_options
        {
          'HurricaneType=TYPE' =>
            'managed by Hurricane (stdio) or standalone (default: tcp)',
          'HurricaneHost=HOST' =>
            'the Hurricane host (default: 127.0.0.1)',
          'HurricanePort=PORT' =>
            'the Hurricane port (default: 3000)',
          'HurricaneProcessGroup=GROUP' =>
            'the Hurricane process group to join (default: http_handler)',
        }
      end

      # Transform the data given by Hurricane into a Rack env dictionary.
      def self.to_rack_hash(data)
        rack_hash = {
          'SCRIPT_NAME' => '',
          'rack.input' => StringIO.new(),
          'rack.errors' => $stderr,
          'rack.multiprocess' => false,
          'rack.multithread' => false,
          'rack.run_once' => false,
          'rack.version' => [1, 0]}
        data.each do |req_tuple|
          key = req_tuple.data[0]
          value = req_tuple.data[1]
          if key.name.eql?('listen_port')
            rack_hash['SERVER_PORT'] = value.to_s()
          elsif key.name.eql?('server_name')
            rack_hash['SERVER_NAME'] = value
          elsif key.name.eql?('scheme')
            rack_hash['rack.url_scheme'] = value.name
          elsif key.name.eql?('version')
            rack_hash['SERVER_PROTOCOL'] = "HTTP/#{value.data[0]}.#{value.data[1]}"
          elsif key.name.eql?('method')
            rack_hash['REQUEST_METHOD'] = value.name
          elsif key.name.eql?('path')
            rack_hash['REQUEST_URI'] = value
            uri = URI(value)
            rack_hash['PATH_INFO'] = uri.path()
            rack_hash['QUERY_STRING'] = uri.query() || ''
          elsif key.name.eql?('headers')
            value.each do |header|
              header_key = header.data[0]
              header_value = header.data[1]
              if header_key.kind_of?(Erlang::Atom)
                header_key = header_key.name.downcase()
              else
                header_key = header_key.downcase()
              end

              if header_key.eql?('content-length')
                rack_hash['CONTENT_LENGTH'] = header_value.to_i()
              elsif header_key == 'content-type'
                rack_hash['CONTENT_TYPE'] = header_value
              end

              header_name = "HTTP_#{header_key.gsub('-', '_').upcase()}"
              rack_hash[header_name] = header_value
            end
          elsif key.name == 'body' and value.kind_of?(Erlang::Binary)
            rack_hash['rack.input'].write(value.data)
            rack_hash['rack.input'].seek(0)
          end
        end

        rack_hash
      end
    end
  end
end

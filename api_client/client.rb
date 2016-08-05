#!/usr/bin/env ruby

require 'net/http'
require 'json'

API_KEY = "104-4312cd6ad95ea4b946d773de63a74b8b"

@last_api_call = Time.now

def api_call(endpoint, method = :get, args = {}, parse = true)
  # stupid rate limiter
  loop do
    break if (Time.now - @last_api_call) > 1
    sleep 1
  end

  @last_api_call = Time.now

  uri = URI(endpoint.start_with?("http://") ? endpoint : "http://2016sv.icfpcontest.org/api/#{endpoint}")

  response = case method
  when :get
    Net::HTTP.start(uri.host, uri.port) do |http|
      request = Net::HTTP::Get.new(uri)
      request["X-API-Key"] = API_KEY
      http.request(request)
    end
  end

  output = case response
  when Net::HTTPSuccess
    body = response.body
    body = JSON.parse(body) if parse
    body
  when Net::HTTPRedirection
    api_call(response['location'], method, args, parse)
  else
    puts response.inspect
  end

  output
end

def blob(hash, parse: true)
  if File.exists?("api_client/blob_cache/#{hash}")
    content = File.open("api_client/blob_cache/#{hash}").read
    content = JSON.parse(content) if parse
    content
  else
    blob = api_call("blob/#{hash}", :get, {}, parse)
    blob = JSON.generate(blob) if parse
    f = File.open("api_client/blob_cache/#{hash}", "w")
    f.write(blob)
    f.close
    blob
  end
end

def snapshots
  api_call("snapshot/list")["snapshots"]
end

def latest_snapshot_hash
  snapshots.sort { |a, b| a["snapshot_time"] > b["snapshot_time"] ? -1 : 1 }.first["snapshot_hash"]
end

def snapshot(hash)
  blob(hash)
end

def problem(hash)
  blob(hash, parse: false)
end

problems = snapshot(latest_snapshot_hash)["problems"]
problems.each do |problem_item|
  problem = problem(problem_item['problem_spec_hash'])
  f = File.open("problems/problem_#{problem_item['problem_id']}", "w")
  f.write(problem)
  f.close
  puts "Problem #{problem_item['problem_id']} written"
end

def run_all_tests
  system('./run-tests.sh')
end

guard :shell do
  watch(%r{.*\.cabal$})          { run_all_tests }
  watch(%r{src/(.+)\.hs$})       { run_all_tests }
  watch(%r{test/.*})  { run_all_tests }
end


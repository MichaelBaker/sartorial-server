namespace :update do
  desc "Update the sartorial repository"
  task :sartorial do
    sh "git subtree pull --prefix vendor/sartorial git@github.com:MichaelBaker/sartorial.git master --squash"
  end

  desc "Update the sartorial-client repository"
  task :sartorial_client do
    sh "git subtree pull --prefix vendor/sartorial-client git@github.com:MichaelBaker/sartorial-client.git master --squash"
  end
end

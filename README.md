# teal.osprey

`osprey` and `teal.osprey` is a ED SPA community community effort to create TLGs and teal modules that have not been created by the NEST team to date and are needed for ED analysis.

# Installation

## Clone and install manually
1. Clone the repository

   The repository can be downloaded directly from the `github.com` site as an archive (see [Github tutorial on cloning to learn more](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository)). Alternatively, Git command line tools offer the same functionality, without the need for manual downloading and unpacking the archive, but require to authenticate to Github. You can authenticate using a key pair or a Personal Access Token (PAT). Please refer to excellent Github tutorials on [connecting to Github using SSH](https://docs.github.com/en/github/authenticating-to-github) or [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).
   1. Using PAT. Input in the Git Bash console, PowerShell or any Linux shell:

      ```
      $ git clone https://github.com/insightsengineering/teal.osprey.git
      Username: your_username_goes_here
      Password: your_token_goes_here
      ```
    1. Using SSH. If set up properly, the repository is ready to be cloned executing:

       ```
       $ git clone https://github.com/insightsengineering/teal.osprey.git
       ```
   This creates a sub-directory `teal.osprey` containing the cloned repository.

2. Build and install

   The native R tools provide a quick way to install a package. Run in PowerShell or any Linux shell:
   ```
   $ R CMD build teal.osprey
   ```
   This command builds the package and creates an archive. The name of the archive is output by the command at then of building. Then input in the shell:
   ```
   $ Rscript -e 'install.packages("name_of_the_archive")
   ```
   Here is an example of a real command (with name_of_the_archive substituted by the output of the build command):
   ```
   $ Rscript -e 'install.packages("teal.osprey_0.1.10.9000.tar.gz")'
   ```


# Contributors:

- **Nina Qi (qit3@gene.com)**
- Chendi Liao (chendi.liao@roche.com)
- Mahdi About (mahdi.about@roche.com)
- Fei Wang (fei.wang.fw6@roche.com)
- Mika Maekinen (mika.maekinen@roche.com)
- Carolyn Zhang (zhang.carolyn@gene.com)

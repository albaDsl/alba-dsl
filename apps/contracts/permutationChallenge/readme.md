To configure the BCHN interface, place a Dhall config file in ~/.alba/cmd.dhall.
Example:

```dhall
let user = "<user>"
let password = "<password>"

in {
    mainnet = {url = "http://localhost:8332/",
               user = user,
               password = password},
    chipnet = {url = "http://localhost:18332/",
               user = user,
               password = password},
}
```

If the BCHN node is running on a separate host, the above config can be used
together with SSH Port Forwarding.

Wallet files now reside in ~/.alba/wallets. The old "wallets" directory can be
manually moved over.

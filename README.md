# chromothripsis_PCAWG [![Build Status](https://travis-ci.com/parklab/chromothripsis_PCAWG.svg?token=EkzyvwdZ2jcY78ErmS88&branch=master)](https://travis-ci.com/parklab/chromothripsis_PCAWG)

### Pre-Reqs: 
- [docker](https://docs.docker.com/engine/installation/)

### Running the app:
- `docker pull scottx611x/chromothripsis-pcawg`
- `docker run -p 3242:3242 scottx611x/chromothripsis-pcawg`
- open http://localhost:3242
- Voila!

### CI/CD:
- New docker images will be built, tagged, and pushed to: [docker hub](https://hub.docker.com/r/scottx611x/chromothripsis-pcawg/) if a PR or TAG are noticed.

### Deployment:
- AWS t2.micro instance free tier
- Accessible at: http://compbio.med.harvard.edu/chromothripsis
- Compbio providing http/ws proxy in `/etc/apache2/sites-available/compbio.med.harvard.edu.conf` to AWS instance:
- Apache proxy conf.
```
        RewriteEngine on
        RewriteCond %{HTTP:Upgrade} =websocket
        RewriteRule /chromothripsis/(.*) ws://54.209.186.16/$1 [P,L]
        RewriteCond %{HTTP:Upgrade} !=websocket
        RewriteRule /chromothripsis/(.*) http://54.209.186.16/$1 [P,L]

        RewriteRule /chromothripsis http://compbio.med.harvard.edu/chromothripsis/

        ProxyPreserveHost On
        ProxyPass /chromothripsis http://54.209.186.16/
        ProxyPassMatch ^/chromothripsis$ http://54.209.186.16
        ProxyPassMatch ^/chromothripsis/(.*)$ http://54.209.186.16/$1
        ProxyPassReverse /chromothripsis http://54.209.186.16
        SetEnv force-proxy-request-1.0 1
        SetEnv proxy-nokeepalive 1
```  
- Docker container w/ run command: 
    + `sudo docker run --rm -d -p 80:3242 -v /var/log/chromothripsis_pcawg_logs/:/var/log/shiny-server/ scottx611x/chromothripsis-pcawg`

### To-Do:
- [ ] Basic tests ( `/` -> `200`)
- [ ] Local development docs
- [x] Deployment Docs
- [ ] Auto deploy new images to AWS on `master` updates

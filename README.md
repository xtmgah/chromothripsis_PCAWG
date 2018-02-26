# chromothripsis_PCAWG [![Build Status](https://travis-ci.com/parklab/chromothripsis_PCAWG.svg?token=EkzyvwdZ2jcY78ErmS88&branch=master)](https://travis-ci.com/parklab/chromothripsis_PCAWG)

[Maintainer Notes](https://github.com/parklab/chromothripsis_PCAWG/wiki/Dev-Notes)

### Pre-Reqs: 
- [docker](https://docs.docker.com/engine/installation/)

### Running the app:
- `docker run -p 3242:3242 scottx611x/chromothripsis-pcawg`
- open http://localhost:3242
- Voila!

### Local Development:
- Make your edits to the application code
- Run `sudo sh build_image.sh` -> you should get a docker image ID in the console if things built properly
- Open http://localhost:3242 and poke around the app
- If you're satisfied with the changes either tag your current commit or create a pull request
- Ask @scottx611x to update the deployed instance with newly created image.

### CI/CD:
- New docker images will be built, tagged, and pushed to: [docker hub](https://hub.docker.com/r/scottx611x/chromothripsis-pcawg/) if a `PR` or `TAG` are noticed.

### Deployment:
- AWS t2.micro instance free tier
- Accessible at: http://compbio.med.harvard.edu/chromothripsis
- Compbio providing http/ws proxy in `/etc/apache2/sites-available/compbio.med.harvard.edu.conf` to AWS instance:
- Apache Proxy Config:
```
   Define chromothripsis_host <hostname>
   RewriteEngine on
   RewriteCond %{HTTP:Upgrade} =websocket
   RewriteRule /chromothripsis/(.*) ws://${chromothripsis_host}/$1 [P,L]
   RewriteCond %{HTTP:Upgrade} !=websocket
   RewriteRule /chromothripsis/(.*) http://${chromothripsis_host}/$1 [P,L]
   RewriteRule /chromothripsis http://compbio.med.harvard.edu/chromothripsis/
   ProxyPreserveHost On
   ProxyPass /chromothripsis http://${chromothripsis_host}/
   ProxyPassMatch ^/chromothripsis$ http://${chromothripsis_host}
   ProxyPassMatch ^/chromothripsis/(.*)$ http://${chromothripsis_host}/$1
   ProxyPassReverse /chromothripsis http://${chromothripsis_host}
   SetEnv force-proxy-request-1.0 1
   SetEnv proxy-nokeepalive 1
```  
- Docker container w/ run command: 
    + `sudo docker run --rm -d -p 80:3242 -v /var/log/chromothripsis_pcawg_logs/:/var/log/shiny-server/ scottx611x/chromothripsis-pcawg`

### To-Do:
- [ ] Basic tests ( `/` -> `200`)
- [x] Local development docs
- [x] Deployment Docs
- [ ] `-v app/:/srv/shinyapps/app`
- [ ] Auto deploy new images to AWS on `master` updates

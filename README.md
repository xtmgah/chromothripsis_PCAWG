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

### To-Do:
- [ ] Run docker behind apache
- [ ] Auto deploy to compbio

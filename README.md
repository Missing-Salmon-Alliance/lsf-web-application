# lsf-web-application
Likely Suspects Framework Data Mobilisation Solution - Web Application to provide interface for data owners to provide data and data users to find data!

## Current Status
### Permanent Branches
- Development branch - main branch, used for ALL edits prior to promotion to Staging branch for testing
- Staging branch - used to test development branch features on shiny server prior to promotion to production site - avoid directly committing to this branch, merge from development only.
- Production Branch - Live Site https://shiny.missingsalmonalliance.org/framework - avoid directly committing to this branch, merge from staging only.
### Temporary Branches
- server-redesign - The current design is poor because database data is loaded on application load. This means that site performance is slow for the landing page. I intend to move this data load to the user login routine and include some waiter displays for more positive UX.

# The Capture of the Academic Industrial Organization Literature


## Resources:
- `aea_jel_codes.xml` is collected from the AEA's [website](https://www.aeaweb.org/econlit/jelCodes.php): 
    -  This can be accessed anywhere by using the following code:
```python
        df = pd.read_xml('aea_jel_codes.xml', xpath='classification')
```

- Each of the journals'.xml files (see `00_Scraping/jel_scopus_matching.ipynb`) are collected from EBSCOHOST's EconLit citation-manager. After running an exclusive year-appropriate (1990-2021), ISSN-based Boolean search, all of the article meta-data can be downloaded by clicking "Share" and then "E-mail a link to download exported results ()". An email will then be sent to the designated address with a download link for the .xml file.

### Reminders
- Absolutely EVERY pandas .csv export (`df.to_csv()`) should *explicitly* note `utf-8` encoding.
- DOI is good for identifying unique matches but it doesn't cover the entire sample. The DOI standard was only introduced in 1998 and its not clear the extent to which DOIs have retroactively been assigned to older documents. For this reason it may be better to use other fields for matching, merging etc.


### Technical Requirements
- Note the requirement for the ioCapture conda environment


#### Pacakges Required For Use (and associated dependencies):
**Python**
- Pandas
- BeautifulSoup4
- lxml
- Requests
- PyPDF2

**R**
- tidyverse
- scales
- stargazer
- pals
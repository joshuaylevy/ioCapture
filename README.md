# The Capture of the Academic Industrial Organization Literature


## Resources:
- `aea_jel_codes.xml` is collected from the AEA's [website](https://www.aeaweb.org/econlit/jelCodes.php): 
    -  This can be accessed anywhere by using the following code:
```python
        df = pd.read_xml('aea_jel_codes.xml',   xpath='classification')
```

- Each of the journals'.xml files (see `00_Scraping/jel_scopus_matching.ipynb`) are collected from EBSCOHOST's EconLit citation-manager. After running an exclusive year-appropriate (1990-2021), ISSN-based Boolean search, all of the article meta-data can be downloaded by clicking "Share" and then "E-mail a link to download exported results ()". An email will then be sent to the designated address with a download link for the .xml file.

### Reminders
- Absolutely EVERY pandas .csv export (`df.to_csv()`) should *explicitly* note `utf-8` encoding.


### Technical Requirements
- Note the requirement for the ioCapture conda environment


##### Pacakges Required For Use (and associated dependencies):
- Pandas
- BeautifulSoup4
- lxml
- Requests

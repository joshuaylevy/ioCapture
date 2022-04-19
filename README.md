# The Capture of the Academic Industrial Organization Literature


## Resources:
- `aea_jel_codes.xml` is collected from the AEA's [website](https://www.aeaweb.org/econlit/jelCodes.php): 
    -  This can be accessed anywhere by using the following code:
```python
        df = pd.read_xml('aea_jel_codes.xml',   xpath='classification')
```

### Reminders
- Absolutely EVERY pandas .csv export (`df.to_csv()`) should *explicitly* note `utf-8` encoding.


### Technical Requirements
- Note the requirement for the ioCapture conda environment


##### Pacakges Required For Use (and associated dependencies):
- Pandas
- BeautifulSoup4
- lxml
- Requests

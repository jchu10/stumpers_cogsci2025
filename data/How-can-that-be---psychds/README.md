# How can that be?

Data and materials were downloaded from "https://childrenhelpingscience.com/exp/studies/3311/responses/all/" on Monday, January 06, 2025 at 01:19 PM. 

## Description

(If you like, you can delete the rest of this file and replace it with a README that describes your specific project and what is contained in this folder!)

This download is structured as a template project folder for your study, How can that be?.  In addition to the data files, it includes the materials used to collect this data (e.g. study ad, study protocol), in the versions that were present when this data was downloaded. All of the data tables that are available from your CHS "All Responses" page (e.g. Frame data, Response overview) are included in this download. 

The `data/` subfolder is organized using Psych-DS, an open source data standard that helps researchers in the behavioral sciences to organize their datasets in a consistent and informative way. You can learn more about Psych-DS by checking out the [documentation](https://psychds-docs.readthedocs.io/en/latest/). However, you don't need to know anything specific about Psych-DS to use this dataset - it still contains regular CSV tabular data files. 

## Contents

Here is a diagram of the directory structure in the folder you have just downloaded: 

```
[STUDY NAME]--[psych-ds]/
      materials/
      data/
           overview/ 
           framedata-per-response/
           raw/
      dataset_description.json
      README.md
      
```

### Materials folder

This folder contains files that represent details about how the study was conducted, which should allow you to reproduce a new version of the CHS study you ran from scratch.

`materials/study_ad_info.json` - A file containing details about the advertisement and recruitment details used to advertise the study on CHS, as of the day this data was downloaded. This information is available whether you ran an internal (Lookit experiment runner) study or an external study. 

If you ran an internal study, you will also have *one* of the following files, depending on whether or not you used the protocol generator:

`materials/study_protocol.json` A file containing the full JSON protocol for your study design.

`materials/study_protocol_generator.js` A file containing the javascript function for generating a study protocol for each participant.

### Data folder

Following the Psych-DS format, the data folder contains only CSV files, except for inside the `raw/` sub-directory.  If you need them, the demographic data table and video files can be downloaded from their respective tabs on your Study Responses page and manually added to this directory.

Note: by default, all uuids used in filenames are truncated, using the first 8 characters of the standard uuids.

#### `overview/`

This subfolder contains two files, one that has one row per response ("Response overview", in the older download system), and one that has one row per child ("Child data"). Both of these files will include whichever of the optional columns indicated by checkboxes you selected.  If you asked for identifiable information (e.g. birth dates, parent's first name), the file names will indicate this. 

So all together, you will see two files, either:

```
overview/
      study-1111aaaa_all-responses_identifiable-true_data.csv
      study-1111aaaa_all-children_identifiable-true_data.csv
```

or:

```
overview/
      study-1111aaaa_all-responses_data.csv
      study-1111aaaa_all-children_data.csv
```

Remember, even if you didn't check any of the boxes for data that CHS knows are identifying, it is up to you to know whether you asked for any other identifying or private information in the rest of your study! 

#### `framedata-per-response`

This subfolder contains frame-by-frame data files for each individual response. The contents of these files are identical to the output of the "frame data" download option:

```
framedata-per-response/
      study-1111aaaa_response-2b2b2b2b_data.csv
      study-1111aaaa_response-3c3c3c3c_data.csv
      study-1111aaaa_response-4d4d4d4d_data.csv
      etc...
```

#### `raw`

This folder contains the raw JSON file from the "All response data" download, which includes every piece of available information about every single response in your dataset, in its original format.

It also includes a placeholder folder where you can put your video files (the other kind of non-CSV data from CHS projects.)  We don't include the videos themselves in the data download in order to keep the size manageable and to avoid sharing this more sensitive data unless you specifically need it. 

So on download, the `raw/` folder will look like this: 

```
raw/
      all-responses-identifiable.json
      video/
            README.md                  <- A simple placeholder text file
```

Separately, you can download your video files and drag them into this folder, i.e.:

```
raw/
      all-responses-identifiable.json
      video/
            README.md
            videoStream_[long-uuid-1]_xx-[framename]_[long-uuid]_xxx.mp4
            videoStream_[long-uuid-2]_yy-[framename]_[long-uuid]_yyy.mp4
            etc...
```

Be careful when sharing datasets in cases where videos (or other identifiable information) should not be included!

### README.md and dataset_description.json 

You are reading the `README.md` file right now! It is a text file with some special formatting to make it a bit easier to read. Including a README file in your project folder is a helpful way to share information about the project with your collaborators (or yourself, for a year from now when you've forgotten what's going on!)

The file named `dataset_description.json` is part of the Psych-DS standard. 

Just like the JSON files you use to create a Lookit internal (experiment runner) study, this is a text file that is structured to be (relatively) easy for both humans and machines to read. It contains metadata about your study, including the name, creator, contact information, and a data dictionary with definitions for all the variables that CHS creates.  

You can open this file in any text editor and add definitions for the variables you created as part of your study protocol (internal studies only).

You can check out the [Psych-DS documentation](https://psychds-docs.readthedocs.io/en/latest/) to learn more about how to keep your data clearly organized and easy for yourself and others to understand! Psych-DS is designed for researchers who may want to share some, but not all, of the data from a scientific project. We hope you will find it useful!

# This file contains functions and definitions to make available
# a group of variables to mkdocs using its plugin mkdocs-macros-plugin,
# in order to integrate a comments system into the static documentation
# site, using the staticman scripts.
#
# Documentation of the components can be found at:
# https://www.mkdocs.org/
# https://mkdocs-macros-plugin.readthedocs.io
# https://staticman.net/
#
# Copyright (C) 2022 Josep M Homs
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/
#
# TODO:
# - Error handling
# - Add support to process json files in addition to yaml

import yaml
from os import listdir
from os.path import join, isfile, isdir
from markupsafe import escape

# Define settings for Staticman comments
commentsPath = "./docs/_comments"
formAction = "https://plutus-community-cors.herokuapp.com/plutus-community-staticman.herokuapp.com/v2/entry/plutus-community/docs/main/comments"
recaptchaSitekey = "6LflJp4fAAAAALfOUV-Uvv3dKr2vF3bxokaMepwx"
recaptchaSecret = "adjYxki/QvUIxJxgnUcIEdFiwBuf08i4peRF8z4rGDx3PqXfjo0Oun7XB4eg5qMkfDxziCG2uUGIGY1LXOEFR2C1uf+Rq8TTqwiAxx9jTv0CyCYk2UAO9UL+CuKzisxSc/xNpxJtaNcYUZ8A6BFAFphwiME4Cvfy7JkHQVaKOdxGz7cC6G/0pHWRlfnO04bLuXAMb0gpmnVbJzJ2YJIg68L/Vf1gWtJ3UhbIifGSd9hcW0iiPv8ZaBokvZmjufb77kmV0b3wbDFGIGY0P58NfrB+gUdzpQ2+TwJVINRW14gVRjFZgoBZPhfZfetLeKBMFB/UV9A/wRYVHdKqcH5g3APtOe6BaXI4NjSo4LmRgLqtfAH5pTEiqYJMjYrNTN29F8v/5AJ5SCTKo2MMC8qhc3ajrkfGBoAYN9LvbFv/c8btHKl3wLs11Oui/DuCHaFqd6GE2t4hvRUJ24+68pE97KkbgpJh15xqFLcnwgiN+7ftFlcSDcuta6twaFtg4HWSpYZingkniXLUVInpKOuaQK+hGnrA6Bm7ACwNUTzj4D8j1eIohj8ok3r6dPS0OE6xI58CoUpZXTW3jOgkQXW17oNr6Ab/pZ6kR9cepESgpI5/4XT1JGWnPTp7WnEsB0S4FJSmigldVF8SN1qyoC1AqnKmRUzKrnw5giIblv69lb8="
# Function to generate a date sorted list with all the comments available
# inside the files (yaml) in the subdirectories of a given directory.
# Subdirectory name is saved as a the "key" field, to be used
# as a filter while rendering the comments for a specific page.
def getComments(path):
    out = []
    for filename in listdir(path):
        f = join(path, filename)
        if isdir(f):
            for filename2 in listdir(f):
                f2 = join(f, filename2)
                if isfile(f2):
                    with open(f2, 'r') as stream:
                        fields=yaml.safe_load(stream)
                    fields["name"] = escape(fields["name"])
                    fields["message"] = escape(fields["message"])
                    fields["email"] = escape(fields["email"])
                    fields["date"] = escape(fields["date"])
                    fields["key"] = filename
                    if not("replying_to_uid" in fields):
                        fields["replying_to_uid"] = "0"
                    out.append(fields)
    return sorted(out, key=lambda d: d['date'])

# Function that receives a flat list of comments and
# returns a nested structure for easier rendering.
def nestedComments(comments, parent="0"):
    nested = []
    for comment in comments:
        if comment['replying_to_uid'] != parent:
            continue
        subdict = comment.copy()
        replies = nestedComments(comments, comment['_id'])
        if replies:
            subdict['replies'] = replies
        nested.append(subdict)
    return nested

# Make the variables available to mkdocs
def define_env(env):
    env.variables.comments = nestedComments(getComments(commentsPath))
    #env.variables.comments = getComments(commentsPath)
    env.variables.formAction = formAction
    env.variables.recaptchaSitekey = recaptchaSitekey
    env.variables.recaptchaSecret = recaptchaSecret

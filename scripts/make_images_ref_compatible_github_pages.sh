#!/bin/bash

find ./docs/ -type f -exec sed -i -e 's/_main/main/g' {} \;
mv docs/_main_files/ docs/main_files/

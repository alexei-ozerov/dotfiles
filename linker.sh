#!/bin/bash

MANIFEST_FILE_PATH=./MANIFEST
DEBUG_MODE=false

# Iterate Over MANIFEST and execute backups and symlinks.
while IFS= read -r LINE; do
    # Check for comments.
    if [[ "${LINE}" == \#* ]]; then
       continue
    elif [[ -z "${LINE}" ]]; then
        continue
    fi

    # Substitute env vars in MANIFEST file.
    SUBBED_LINE=$(echo "${LINE}" | envsubst)

    if "${DEBUG_MODE}" == "true"; then echo "[DEBUG] Processing ${SUBBED_LINE}."; fi

    # Split up line values.
    TARGET=$(echo "${SUBBED_LINE}" | cut -d "|" -f 1)
    STATE=$(echo "${SUBBED_LINE}" | cut -d "|" -f 2)
    MODE=$(echo "${SUBBED_LINE}" | cut -d "|" -f 3)
    DESTINATION=$(echo "${SUBBED_LINE}" | cut -d "|" -f 4)

    # Check for invalid fields.
    if [[ -z "${TARGET}" ]]; then
        echo "[ERROR] TARGET field not set, skipping."
        continue
    elif [[ -z "${STATE}" ]]; then
        echo "[ERROR] STATE field not set, skipping."
        continue
    elif [[ -z "${MODE}" ]]; then
        echo "[ERROR]MODE field not set, skipping"
        continue
    elif [[ -z "${DESTINATION}" ]]; then
        echo "[ERROR] DESTINATION field not set, skipping."
        continue
    fi

    # Check for '/' at the end of $DESTINATION and trim if present. 
    if [[ "${DESTINATION}" == *\/ ]]; then
        if "${DEBUG_MODE}" == "true"; then echo "[DEBUG] Removing / from the end of ${DESTINATION}."; fi
        DESTINATION=${DESTINATION%?}
        if "${DEBUG_MODE}" == "true"; then echo "[DEBUG] Removed: ${DESTINATION}."; fi
    fi

    # Create symlinks.
    if [[ "${MODE}" == "overwrite" ]]; then
        echo "[INFO]  Creating symlink from ${TARGET} to ${DESTINATION}."
        #ln -sf "${TARGET}" "${DESTINATION}/${TARGET}"
    elif [[ "${MODE}" == "backup" ]]; then
        FILE_BACKUP_LOCATION="${DESTINATION}/${TARGET}"

        # Check if $FILE_BACKUP_LOCATION is not a directory.
        if [ ! -d "${FILE_BACKUP_LOCATION}" ]; then
            # If not a directory, check if $FILE_BACKUP_LOCATION is a file.
            if [ ! -f "${FILE_BACKUP_LOCATION}" ]; then
                echo "[WARN]  File ${FILE_BACKUP_LOCATION} not found, not creating backup."
                echo "[INFO]  Creating symlink from ${TARGET} to ${DESTINATION}/${TARGET}."
                #ln -sf "${TARGET}" "${DESTINATION}/${TARGET}"
            elif [ ! -L "${FILE_BACKUP_LOCATION}" ]; then
                echo "[WARN]  Symlink ${FILE_BACKUP_LOCATION} not found, not creating backup."
                echo "[INFO]  Creating symlink from ${TARGET} to ${DESTINATION}/${TARGET}."
                #ln -sf "${TARGET}" "${DESTINATION}/${TARGET}"
            fi
        fi

        # If file or directory exists, take a backup.
        if [ ! -d "backups" ]; then
            mkdir backups
        fi

        TIMESTAMP_PREFIX=$(date +%Y%m%d_%H%M%S)
        BACKUP_LOCATION="backups/${TIMESTAMP_PREFIX}_bak"
        
        mkdir -p "${BACKUP_LOCATION}"

        echo "[INFO]  Creating a backup at ${BACKUP_LOCATION} for ${DESTINATION}/${TARGET}."
        cp -r --dereference "${DESTINATION}/${TARGET}" "${BACKUP_LOCATION}/."

        echo "[INFO]  Creating symlink from ${TARGET} to ${DESTINATION}/${TARGET}."
        #ln -sf "${TARGET}" "${DESTINATION}/${TARGET}"
    else
        echo "[WARN]  No valid mode found, skipping line."
    fi
done < "${MANIFEST_FILE_PATH}"

exit 0

/*
 * Copyright 2025 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.attribute;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class LegacyAttributeConverter {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String LEGACY_ATTRIBUTES_TAG = "Attributes";
  private static final String LEGACY_NAME_TAG = "Name";
  private static final String LEGACY_BASE_TAG = "Base";
  private static final String ATTRIBUTES_TAG = "attributes";
  private static final String ID_TAG = "id";
  private static final String BASE_TAG = "base";

  private LegacyAttributeConverter() {}

  public static boolean convertLegacyAttributes(CompoundTag compoundTag) {
    if (compoundTag == null
        || !compoundTag.contains(LEGACY_ATTRIBUTES_TAG, Tag.TAG_LIST)
        || compoundTag.contains(ATTRIBUTES_TAG, Tag.TAG_LIST)) {
      return false;
    }

    ListTag legacyList = compoundTag.getList(LEGACY_ATTRIBUTES_TAG, Tag.TAG_COMPOUND);
    ListTag convertedList = new ListTag();
    for (int index = 0; index < legacyList.size(); index++) {
      CompoundTag legacyEntry = legacyList.getCompound(index);
      if (!legacyEntry.contains(LEGACY_NAME_TAG, Tag.TAG_STRING)) {
        continue;
      }
      CompoundTag convertedEntry = new CompoundTag();
      convertedEntry.putString(ID_TAG, legacyEntry.getString(LEGACY_NAME_TAG));
      if (legacyEntry.contains(LEGACY_BASE_TAG, Tag.TAG_ANY_NUMERIC)) {
        convertedEntry.putDouble(BASE_TAG, legacyEntry.getDouble(LEGACY_BASE_TAG));
      }
      convertedList.add(convertedEntry);
    }

    compoundTag.remove(LEGACY_ATTRIBUTES_TAG);
    compoundTag.put(ATTRIBUTES_TAG, convertedList);
    log.info("Converted {} legacy entity attribute(s) to 1.21 format.", convertedList.size());
    return true;
  }
}

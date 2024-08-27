/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.data.display;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.nbt.CompoundTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record DisplayAttributeEntry(
    DisplayAttributeType displayAttributeType, boolean booleanValue, int intValue) {

  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_BOOLEAN_VALUE_TAG = "Boolean";
  public static final String DATA_INT_VALUE_TAG = "Int";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public DisplayAttributeEntry(CompoundTag compoundTag) {
    this(
        DisplayAttributeType.get(compoundTag.getString(DATA_TYPE_TAG)),
        compoundTag.getBoolean(DATA_BOOLEAN_VALUE_TAG),
        compoundTag.contains(DATA_INT_VALUE_TAG) ? compoundTag.getInt(DATA_INT_VALUE_TAG) : 0);
  }

  public DisplayAttributeEntry create(CompoundTag compoundTag) {
    return new DisplayAttributeEntry(compoundTag);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putString(DATA_TYPE_TAG, displayAttributeType.name());
    compoundTag.putBoolean(DATA_BOOLEAN_VALUE_TAG, booleanValue);
    if (intValue != 0) {
      compoundTag.putInt(DATA_INT_VALUE_TAG, intValue);
    }
    return compoundTag;
  }

  public CompoundTag createTag() {
    return write(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DisplayAttributeEntry{"
        + "displayAttributeType="
        + displayAttributeType
        + ", boolean="
        + booleanValue
        + ", int="
        + intValue
        + '}';
  }
}

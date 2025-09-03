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

public record DisplayAttributeEntry(boolean booleanValue, int intValue, String stringValue) {

  public static final String DATA_BOOLEAN_VALUE_TAG = "Bool";
  public static final String DATA_INT_VALUE_TAG = "Int";
  public static final String DATA_STRING_VALUE_TAG = "Text";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public DisplayAttributeEntry(final boolean booleanValue) {
    this(booleanValue, 0, "");
  }

  public DisplayAttributeEntry(final int intValue) {
    this(false, intValue, "");
  }

  public DisplayAttributeEntry(final String stringValue) {
    this(false, 0, stringValue);
  }

  public DisplayAttributeEntry(final CompoundTag compoundTag) {
    this(
        compoundTag.contains(DATA_BOOLEAN_VALUE_TAG)
            && compoundTag.getBoolean(DATA_BOOLEAN_VALUE_TAG),
        compoundTag.contains(DATA_INT_VALUE_TAG) ? compoundTag.getInt(DATA_INT_VALUE_TAG) : 0,
        compoundTag.contains(DATA_STRING_VALUE_TAG)
            ? compoundTag.getString(DATA_STRING_VALUE_TAG)
            : "");
  }

  public DisplayAttributeEntry create(final CompoundTag compoundTag) {
    return new DisplayAttributeEntry(compoundTag);
  }

  public CompoundTag write(final CompoundTag compoundTag) {
    if (booleanValue) {
      compoundTag.putBoolean(DATA_BOOLEAN_VALUE_TAG, true);
    }
    if (intValue != 0) {
      compoundTag.putInt(DATA_INT_VALUE_TAG, intValue);
    }
    if (stringValue != null && !stringValue.isEmpty()) {
      compoundTag.putString(DATA_STRING_VALUE_TAG, stringValue);
    }
    return compoundTag;
  }

  public CompoundTag createTag() {
    return write(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DisplayAttributeEntry{"
        + "boolean="
        + booleanValue
        + ", int="
        + intValue
        + ", string="
        + stringValue
        + '}';
  }
}

/*
 * Copyright 2026 Markus Bordihn
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AttributeTagUtilsTest {

  private static final String TAG_NAME = "Attribute";

  @Test
  @DisplayName("Only an enabled flag is stored")
  void testOnlyEnabledFlagIsStored() {
    CompoundTag compoundTag = new CompoundTag();

    AttributeTagUtils.putIfTrue(compoundTag, TAG_NAME, false);
    assertFalse(compoundTag.contains(TAG_NAME));

    AttributeTagUtils.putIfTrue(compoundTag, TAG_NAME, true);
    assertTrue(compoundTag.getBooleanOr(TAG_NAME, false));
  }

  @Test
  @DisplayName("Only a value other than zero is stored")
  void testOnlyNonZeroValueIsStored() {
    CompoundTag compoundTag = new CompoundTag();

    AttributeTagUtils.putIfNotZero(compoundTag, TAG_NAME, 0.0D);
    assertFalse(compoundTag.contains(TAG_NAME));

    AttributeTagUtils.putIfNotZero(compoundTag, TAG_NAME, -1.5D);
    assertEquals(-1.5D, compoundTag.getDoubleOr(TAG_NAME, 0.0D));
  }
}

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

package de.markusbordihn.easynpc.data.attribute;

import net.minecraft.nbt.CompoundTag;

public record EnvironmentalAttributes(
    boolean canFloat, boolean canBreatheUnderwater, boolean freefall)
    implements EntityAttributesInterface {

  public static final String FREEFALL_TAG = EnvironmentalAttributeType.FREEFALL.getTagName();
  public static final String FLOAT_TAG = EnvironmentalAttributeType.CAN_FLOAT.getTagName();
  public static final String BREATHE_UNDERWATER_TAG =
      EnvironmentalAttributeType.CAN_BREATHE_UNDERWATER.getTagName();

  public EnvironmentalAttributes() {
    this(false, false, false);
  }

  public static EnvironmentalAttributes decode(CompoundTag compoundTag) {
    return new EnvironmentalAttributes(
        compoundTag.getBoolean(BREATHE_UNDERWATER_TAG),
        compoundTag.getBoolean(FLOAT_TAG),
        compoundTag.getBoolean(FREEFALL_TAG));
  }

  public EnvironmentalAttributes withCanBreathUnderwater(boolean canBreathUnderwater) {
    return new EnvironmentalAttributes(this.canFloat, canBreathUnderwater, this.freefall);
  }

  public EnvironmentalAttributes withCanFloat(boolean canFloat) {
    return new EnvironmentalAttributes(canFloat, this.canBreatheUnderwater, this.freefall);
  }

  public EnvironmentalAttributes withFreefall(boolean freefall) {
    return new EnvironmentalAttributes(this.canFloat, this.canBreatheUnderwater, freefall);
  }

  public CompoundTag encode(CompoundTag compoundTag) {
    compoundTag.putBoolean(BREATHE_UNDERWATER_TAG, this.canBreatheUnderwater);
    compoundTag.putBoolean(FLOAT_TAG, this.canFloat);
    compoundTag.putBoolean(FREEFALL_TAG, this.freefall);
    return compoundTag;
  }
}

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

package de.markusbordihn.easynpc.data.display;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class DisplayAttributeDataSetTest {

  private static ListTag createListTag(DisplayAttributeType attributeType, boolean value) {
    ListTag listTag = new ListTag();
    CompoundTag entryTag = new CompoundTag();
    entryTag.putString("Type", attributeType.name());
    new DisplayAttributeEntry(value).write(entryTag);
    listTag.add(entryTag);
    return listTag;
  }

  @ParameterizedTest
  @EnumSource(
      value = DisplayAttributeType.class,
      names = {"NONE"},
      mode = EnumSource.Mode.EXCLUDE)
  @DisplayName("Every attribute is present in a default data set")
  void defaultDataSetContainsAllAttributes(DisplayAttributeType attributeType) {
    assertTrue(DisplayAttributeDataSet.createDefault().hasAttribute(attributeType));
  }

  @Test
  @DisplayName("Partial data from an older version keeps the defaults for missing attributes")
  void partialDataKeepsDefaultsForMissingAttributes() {
    DisplayAttributeDataSet dataSet =
        new DisplayAttributeDataSet(createListTag(DisplayAttributeType.VISIBLE_AT_NIGHT, false));

    assertFalse(dataSet.getAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT).booleanValue());
    assertTrue(dataSet.getAttribute(DisplayAttributeType.VISIBLE_AT_DAY).booleanValue());
    assertTrue(dataSet.getAttribute(DisplayAttributeType.VISIBLE_IN_STANDARD).booleanValue());
    assertTrue(dataSet.getAttribute(DisplayAttributeType.VISIBLE_TO_OWNER).booleanValue());
    assertEquals(7, dataSet.getAttribute(DisplayAttributeType.LIGHT_LEVEL).intValue());
  }

  @Test
  @DisplayName("An empty attribute map is completed with the defaults")
  void emptyAttributeMapIsCompletedWithDefaults() {
    DisplayAttributeDataSet dataSet =
        new DisplayAttributeDataSet(new EnumMap<>(DisplayAttributeType.class));

    assertTrue(dataSet.getAttribute(DisplayAttributeType.VISIBLE).booleanValue());
    assertTrue(dataSet.getAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT).booleanValue());
    assertEquals(
        NameVisibilityType.ALWAYS.toString(),
        dataSet.getAttribute(DisplayAttributeType.NAME_VISIBILITY).stringValue());
  }

  @Test
  @DisplayName("Disabled attributes survive a save and reload cycle")
  void disabledAttributesSurviveSaveAndReload() {
    DisplayAttributeDataSet dataSet =
        DisplayAttributeDataSet.createDefault()
            .withAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT, new DisplayAttributeEntry(false));

    DisplayAttributeDataSet reloadedDataSet = new DisplayAttributeDataSet(dataSet.save());

    assertFalse(reloadedDataSet.getAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT).booleanValue());
    assertTrue(reloadedDataSet.getAttribute(DisplayAttributeType.VISIBLE_AT_DAY).booleanValue());
  }

  @Test
  @DisplayName("A fully transparent opacity survives a save and reload cycle")
  void fullyTransparentOpacitySurvivesSaveAndReload() {
    DisplayAttributeDataSet dataSet =
        DisplayAttributeDataSet.createDefault()
            .withAttribute(
                DisplayAttributeType.OPACITY,
                new DisplayAttributeEntry(DisplayAttributeType.MIN_OPACITY));

    DisplayAttributeDataSet reloadedDataSet = new DisplayAttributeDataSet(dataSet.save());

    assertEquals(
        DisplayAttributeType.MIN_OPACITY,
        reloadedDataSet.getAttribute(DisplayAttributeType.OPACITY).intValue());
  }

  @Test
  @DisplayName("An unchanged data set keeps the default opacity out of the saved tag")
  void unchangedDataSetKeepsTheDefaultOpacityOutOfTheSavedTag() {
    for (Tag entry : DisplayAttributeDataSet.createDefault().save()) {
      assertFalse(
          DisplayAttributeType.OPACITY.name().equals(((CompoundTag) entry).getString("Type")),
          "A default opacity must not be written");
    }
  }
}

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

package de.markusbordihn.easynpc.data.test;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.markusbordihn.easynpc.component.DataComponents;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

public record TestItemData(String quote, int numberValue, int powerLevel, int usageCounter) {

  public static final String ID = "test_item_data";
  public static final String DEFAULT_QUOTE = "This is a test quote to verify custom data!";
  public static final int DEFAULT_NUMBER_VALUE = 42;
  public static final int DEFAULT_POWER_LEVEL = 9001;
  public static final int DEFAULT_USAGE_COUNTER = 0;

  public static final TestItemData DEFAULT =
      new TestItemData(
          DEFAULT_QUOTE, DEFAULT_NUMBER_VALUE, DEFAULT_POWER_LEVEL, DEFAULT_USAGE_COUNTER);

  public static final Codec<TestItemData> CODEC =
      RecordCodecBuilder.create(
          instance ->
              instance
                  .group(
                      Codec.STRING.fieldOf("quote").forGetter(TestItemData::quote),
                      Codec.INT.fieldOf("numberValue").forGetter(TestItemData::numberValue),
                      Codec.INT.fieldOf("powerLevel").forGetter(TestItemData::powerLevel),
                      Codec.INT.fieldOf("usageCounter").forGetter(TestItemData::usageCounter))
                  .apply(instance, TestItemData::new));

  public static final StreamCodec<RegistryFriendlyByteBuf, TestItemData> STREAM_CODEC =
      StreamCodec.composite(
          ByteBufCodecs.STRING_UTF8,
          TestItemData::quote,
          ByteBufCodecs.INT,
          TestItemData::numberValue,
          ByteBufCodecs.INT,
          TestItemData::powerLevel,
          ByteBufCodecs.INT,
          TestItemData::usageCounter,
          TestItemData::new);

  public static boolean has(ItemStack itemStack) {
    return itemStack != null
        && !itemStack.isEmpty()
        && itemStack.has(DataComponents.TEST_ITEM_DATA);
  }

  public static TestItemData get(ItemStack itemStack) {
    if (!has(itemStack)) {
      return null;
    }
    return itemStack.get(DataComponents.TEST_ITEM_DATA);
  }

  public static ItemStack set(Item item, TestItemData testItemData) {
    return set(new ItemStack(item), testItemData);
  }

  public static ItemStack set(ItemStack itemStack, TestItemData testItemData) {
    if (itemStack == null || itemStack.isEmpty() || testItemData == null) {
      return null;
    }
    itemStack.set(DataComponents.TEST_ITEM_DATA, testItemData);
    return itemStack;
  }

  public TestItemData withIncrementedUsageCounter() {
    return new TestItemData(this.quote, this.numberValue, this.powerLevel, this.usageCounter + 1);
  }
}

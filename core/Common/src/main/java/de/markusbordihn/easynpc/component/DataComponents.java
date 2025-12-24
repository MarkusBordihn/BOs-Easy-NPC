/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.easynpc.component;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.test.TestItemData;
import java.util.function.Supplier;
import net.minecraft.core.Registry;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DataComponents {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static DataComponentType<PresetData> PRESET_DATA;
  public static DataComponentType<TestItemData> TEST_ITEM_DATA;

  private DataComponents() {}

  public static void registerDataComponents() {
    log.info("{} Data Components ...", Constants.MOD_NAME);
    PRESET_DATA =
        Registry.register(
            BuiltInRegistries.DATA_COMPONENT_TYPE,
            ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, PresetData.ID),
            DataComponentType.<PresetData>builder()
                .persistent(PresetData.CODEC)
                .networkSynchronized(PresetData.STREAM_CODEC)
                .build());
    TEST_ITEM_DATA =
        Registry.register(
            BuiltInRegistries.DATA_COMPONENT_TYPE,
            ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, TestItemData.ID),
            DataComponentType.<TestItemData>builder()
                .persistent(TestItemData.CODEC)
                .networkSynchronized(TestItemData.STREAM_CODEC)
                .build());
  }

  public static void registerPresetData(Supplier<DataComponentType<PresetData>> supplier) {
    log.info("{} Preset Data Component {} ...", Constants.MOD_NAME, supplier.get());
    PRESET_DATA = supplier.get();
  }

  public static void registerTestItemData(Supplier<DataComponentType<TestItemData>> supplier) {
    log.info("{} Test Item Data Component {} ...", Constants.MOD_NAME, supplier.get());
    TEST_ITEM_DATA = supplier.get();
  }
}

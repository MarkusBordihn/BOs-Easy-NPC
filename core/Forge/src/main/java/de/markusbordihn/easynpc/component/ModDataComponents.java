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

package de.markusbordihn.easynpc.component;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.test.TestItemData;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.Registries;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class ModDataComponents {

  public static final DeferredRegister<DataComponentType<?>> DATA_COMPONENTS =
      DeferredRegister.create(Registries.DATA_COMPONENT_TYPE, Constants.MOD_ID);
  public static final RegistryObject<DataComponentType<PresetData>> PRESET_DATA =
      DATA_COMPONENTS.register(
          PresetData.ID,
          () ->
              DataComponentType.<PresetData>builder()
                  .persistent(PresetData.CODEC)
                  .networkSynchronized(PresetData.STREAM_CODEC)
                  .build());
  public static final RegistryObject<DataComponentType<TestItemData>> TEST_ITEM_DATA =
      DATA_COMPONENTS.register(
          TestItemData.ID,
          () ->
              DataComponentType.<TestItemData>builder()
                  .persistent(TestItemData.CODEC)
                  .networkSynchronized(TestItemData.STREAM_CODEC)
                  .build());

  private ModDataComponents() {}

  @SubscribeEvent
  public static void onCommonSetup(FMLCommonSetupEvent event) {
    event.enqueueWork(
        () -> {
          DataComponents.registerPresetData(PRESET_DATA);
          DataComponents.registerTestItemData(TEST_ITEM_DATA);
        });
  }
}

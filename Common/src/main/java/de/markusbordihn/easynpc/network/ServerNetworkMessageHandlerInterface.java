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

package de.markusbordihn.easynpc.network;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.spawner.SpawnerSettingType;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface ServerNetworkMessageHandlerInterface {
  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  void triggerActionEvent(UUID uuid, ActionEventType actionEventType);

  void triggerDialogButtonAction(UUID uuid, UUID dialogId, UUID buttonId);

  void changeSpawnerSettings(BlockPos blockPos, SpawnerSettingType spawnerSettingType, int value);

  void importPreset(UUID uuid, PresetType presetType, ResourceLocation resourceLocation);

  void importPreset(UUID uuid, PresetType presetType, CompoundTag compoundTag);

  default void importCustomPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.CUSTOM, resourceLocation);
  }

  default void importDataPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.DATA, resourceLocation);
  }

  default void importDefaultPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.DEFAULT, resourceLocation);
  }

  default void importLocalPreset(UUID uuid, CompoundTag compoundTag) {
    importPreset(uuid, PresetType.LOCAL, compoundTag);
  }

  default void importWorldPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.WORLD, resourceLocation);
  }
}

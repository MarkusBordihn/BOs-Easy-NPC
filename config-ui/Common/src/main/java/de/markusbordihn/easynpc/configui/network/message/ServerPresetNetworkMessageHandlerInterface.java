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

package de.markusbordihn.easynpc.configui.network.message;

import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.ExportCustomPresetServerMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ExportPresetMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ExportWorldPresetMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ImportPresetMessage;
import de.markusbordihn.easynpc.configui.network.message.server.SpawnPresetMessage;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetExportFormat;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.preset.PresetType;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;

public interface ServerPresetNetworkMessageHandlerInterface {

  default void importPreset(UUID uuid, PresetType presetType, ResourceLocation resourceLocation) {
    if (uuid != null && presetType != null && resourceLocation != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ImportPresetMessage(uuid, presetType, null, resourceLocation));
    }
  }

  default void importPreset(
      UUID uuid,
      PresetType presetType,
      CompoundTag compoundTag,
      ResourceLocation resourceLocation) {
    if (uuid != null && presetType != null && compoundTag != null && !compoundTag.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(
          new ImportPresetMessage(uuid, presetType, compoundTag, resourceLocation));
    }
  }

  default void exportPreset(UUID uuid, String name) {
    exportPreset(uuid, name, PresetExportFormat.getDefault(), PresetMetadata.getDefault());
  }

  default void exportPreset(
      UUID uuid, String name, PresetExportFormat exportFormat, PresetMetadata metadata) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(
          new ExportPresetMessage(uuid, name, exportFormat, metadata));
    }
  }

  default void exportWorldPreset(UUID uuid, String name, PresetMetadata metadata) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(new ExportWorldPresetMessage(uuid, name, metadata));
    }
  }

  default void exportCustomPreset(UUID uuid, String name, PresetMetadata metadata) {
    if (uuid != null && name != null && !name.isEmpty()) {
      NetworkHandlerManager.sendMessageToServer(
          new ExportCustomPresetServerMessage(uuid, name, metadata));
    }
  }

  default void importCustomPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.CUSTOM, resourceLocation);
  }

  default void importDefaultPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.DEFAULT, resourceLocation);
  }

  default void importLocalPreset(
      UUID uuid, CompoundTag compoundTag, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.LOCAL, compoundTag, resourceLocation);
  }

  default void importWorldPreset(UUID uuid, ResourceLocation resourceLocation) {
    importPreset(uuid, PresetType.WORLD, resourceLocation);
  }

  default void spawnPreset(
      PresetType presetType, ResourceLocation resourceLocation, boolean useOriginalData) {
    if (presetType != null && resourceLocation != null) {
      NetworkHandlerManager.sendMessageToServer(
          new SpawnPresetMessage(presetType, resourceLocation, useOriginalData, null));
    }
  }

  default void spawnPresetWithData(PresetData presetData, boolean useOriginalData) {
    if (presetData != null && presetData.hasValidData()) {
      NetworkHandlerManager.sendMessageToServer(
          new SpawnPresetMessage(
              presetData.presetType(), presetData.location(), useOriginalData, presetData.data()));
    }
  }
}

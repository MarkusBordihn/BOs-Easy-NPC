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
import de.markusbordihn.easynpc.data.cache.CacheType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.DefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface ClientNetworkMessageHandlerInterface {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  void exportClientPreset(UUID uuid, String name, ServerPlayer serverPlayer);

  void syncCacheData(UUID uuid, ServerPlayer serverPlayer, CacheType cacheType, CompoundTag data);

  default void syncActionEventSet(UUID uuid, ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC != null && easyNPC.getEasyNPCActionEventData() != null) {
      syncCacheData(
          uuid,
          serverPlayer,
          CacheType.ACTION_DATA_SET,
          easyNPC.getEasyNPCActionEventData().getActionEventSet().createTag());
    } else {
      log.error("Failed to sync action event set for entity with UUID {}", uuid);
    }
  }

  default void syncDialogDataSet(UUID uuid, ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC != null && easyNPC.getEasyNPCDialogData() != null) {
      syncCacheData(
          uuid,
          serverPlayer,
          CacheType.DIALOG_DATA_SET,
          easyNPC.getEasyNPCDialogData().getDialogDataSet().createTag());
    } else {
      log.error("Failed to sync dialog data set for entity with UUID {}", uuid);
    }
  }

  default void syncObjectiveDataSet(UUID uuid, ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC != null && easyNPC.getEasyNPCObjectiveData() != null) {
      syncCacheData(
          uuid,
          serverPlayer,
          CacheType.OBJECTIVE_DATA_SET,
          easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet().createTag());
    } else {
      log.error("Failed to sync objective data set for entity with UUID {}", uuid);
    }
  }

  default void syncCustomPresets(ServerPlayer serverPlayer) {
    Set<ResourceLocation> customPresets =
        CustomPresetDataFiles.getPresetResourceLocations().collect(Collectors.toSet());
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put("customPresets", CompoundTagUtils.writeResourceLocations(customPresets));
    syncCacheData(Constants.EMPTY_UUID, serverPlayer, CacheType.CUSTOM_PRESETS, compoundTag);
  }

  default void syncDefaultPresets(ServerPlayer serverPlayer) {
    Set<ResourceLocation> defaultPresets =
        DefaultPresetDataFiles.getPresetResourceLocations(serverPlayer.getServer())
            .collect(Collectors.toSet());
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put("defaultPresets", CompoundTagUtils.writeResourceLocations(defaultPresets));
    syncCacheData(Constants.EMPTY_UUID, serverPlayer, CacheType.DEFAULT_PRESETS, compoundTag);
  }

  default void syncWorldPresets(ServerPlayer serverPlayer) {
    Set<ResourceLocation> worldPresets =
        WorldPresetDataFiles.getPresetResourceLocations().collect(Collectors.toSet());
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put("worldPresets", CompoundTagUtils.writeResourceLocations(worldPresets));
    syncCacheData(Constants.EMPTY_UUID, serverPlayer, CacheType.WORLD_PRESETS, compoundTag);
  }
}

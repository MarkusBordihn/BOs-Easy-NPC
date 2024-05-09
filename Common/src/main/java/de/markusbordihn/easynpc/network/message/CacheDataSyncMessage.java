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

package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.cache.CacheManager;
import de.markusbordihn.easynpc.data.cache.CacheType;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.network.NetworkMessage;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import io.netty.buffer.Unpooled;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class CacheDataSyncMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "cache_data_sync");

  private final CompoundTag data;
  private final CacheType cacheType;

  public CacheDataSyncMessage(UUID uuid, CacheType cacheType, CompoundTag data) {
    super(uuid);
    this.cacheType = cacheType;
    this.data = data;
  }

  public static CacheDataSyncMessage decode(final FriendlyByteBuf buffer) {
    return new CacheDataSyncMessage(
        buffer.readUUID(), buffer.readEnum(CacheType.class), buffer.readNbt());
  }

  public static FriendlyByteBuf encode(
      final CacheDataSyncMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.cacheType);
    buffer.writeNbt(message.data);
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(CacheDataSyncMessage message, ServerPlayer serverPlayer) {
    UUID uuid = message.getUUID();
    if (uuid == null || serverPlayer != null) {
      return;
    }

    // Verify cache type
    CacheType cacheType = message.getCacheType();
    if (cacheType == null) {
      log.error("Unable to get valid cache type for {}", uuid);
      return;
    }

    // Verify data
    CompoundTag data = message.getData();
    if (data == null || data.isEmpty()) {
      log.error("Unable to get valid data {} for {}", data, uuid);
      return;
    }

    // Handle cache data sync
    switch (cacheType) {
      case ACTION_DATA_SET -> {
        ActionEventSet actionEventSet = new ActionEventSet(data);
        log.info("Update action event set for {} with {}", uuid, actionEventSet);
        CacheManager.setActionDataSet(uuid, actionEventSet);
      }
      case DIALOG_DATA_SET -> {
        DialogDataSet dialogDataSet = new DialogDataSet(data);
        log.info("Update dialog data set for {} with {}", uuid, dialogDataSet);
        CacheManager.setDialogDataSet(uuid, dialogDataSet);
      }
      case OBJECTIVE_DATA_SET -> {
        ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet(data);
        log.info("Update objective data set for {} with {}", uuid, objectiveDataSet);
        CacheManager.setObjectiveDataSet(uuid, objectiveDataSet);
      }
      case CUSTOM_PRESETS -> {
        log.info("Update custom presets with {}", data);
        Set<ResourceLocation> customPresets =
            CompoundTagUtils.readResourceLocations(data.getList("customPresets", 10));
        CacheManager.setCustomPresets(customPresets);
      }
      case WORLD_PRESETS -> {
        log.info("Update world presets with {}", data);
        Set<ResourceLocation> worldPresets =
            CompoundTagUtils.readResourceLocations(data.getList("worldPresets", 10));
        CacheManager.setWorldPresets(worldPresets);
      }
      case DEFAULT_PRESETS -> {
        log.info("Update default presets with {}", data);
        Set<ResourceLocation> defaultPresets =
            CompoundTagUtils.readResourceLocations(data.getList("defaultPresets", 10));
        CacheManager.setDefaultPresets(defaultPresets);
      }
      default -> log.error("Unknown cache type {} for {}", cacheType, uuid);
    }
  }

  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public CompoundTag getData() {
    return data;
  }

  public CacheType getCacheType() {
    return cacheType;
  }
}

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

package de.markusbordihn.easynpc.data.server;

import de.markusbordihn.easynpc.Constants;
import java.util.EnumMap;
import java.util.EnumSet;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ServerEntityData {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Custom Entity Data]";
  private static final EnumSet<ServerDataIndex> usedCustomDataAccessorIdSet =
      EnumSet.noneOf(ServerDataIndex.class);
  private static int customDataAccessorId = -1;
  private final EnumMap<ServerDataIndex, ServerDataItem<?>> customEntityDataMap =
      new EnumMap<>(ServerDataIndex.class);
  private final boolean isClientSide;

  public ServerEntityData(Entity entity) {
    this.isClientSide = entity != null && entity.level().isClientSide;
  }

  /*
   * Auto-generated id, should be mostly used for testing purpose.
   * For production use, please define a custom index instead!
   */
  public static <T> ServerDataAccessor<T> defineId(EntityDataSerializer<T> entityDataSerializers) {
    // Check if we have a free custom data accessor id.
    if (customDataAccessorId >= ServerDataIndex.MAX_FREE_INDEX) {
      log.error(
          "{} No more custom data accessor available for {} with id {}"
              + "The maximum for auto-generated ids is {}",
          LOG_PREFIX,
          entityDataSerializers,
          customDataAccessorId,
          ServerDataIndex.MAX_FREE_INDEX);
      return null;
    }
    log.warn("{} Please define a custom index instead of using the auto-generated.", LOG_PREFIX);

    return defineId(ServerDataIndex.getIndex(++customDataAccessorId), entityDataSerializers);
  }

  public static <T> ServerDataAccessor<T> defineId(
      ServerDataIndex serverDataIndex, EntityDataSerializer<T> entityDataSerializers) {

    // Make sure that we have a valid custom data accessor id, and it is not already in use.
    if (usedCustomDataAccessorIdSet.contains(serverDataIndex)) {
      log.error(
          "{} Can't define custom data accessor {} with id {}, because it is already in use!",
          LOG_PREFIX,
          entityDataSerializers,
          serverDataIndex.ordinal());
      return null;
    }

    ServerDataAccessor<T> serverDataAccessor =
        new ServerDataAccessor<>(serverDataIndex, entityDataSerializers);
    log.debug(
        "{} Create custom data accessor {} with id {}",
        LOG_PREFIX,
        serverDataAccessor,
        serverDataIndex);
    usedCustomDataAccessorIdSet.add(serverDataIndex);
    return serverDataAccessor;
  }

  public <T> void define(ServerDataAccessor<T> serverDataAccessor, T customData) {
    if (!this.isClientSide) {
      ServerDataItem<T> dataItem = new ServerDataItem<>(serverDataAccessor, customData);
      log.debug("{} Define custom data item {} with {}", LOG_PREFIX, dataItem, serverDataAccessor);
      this.customEntityDataMap.put(serverDataAccessor.getIndex(), dataItem);
    }
  }

  public <T> void set(ServerDataAccessor<T> entityDataAccessor, T customData) {
    ServerDataItem<T> serverDataItem = this.getDataItem(entityDataAccessor);
    if (serverDataItem != null) {
      log.debug(
          "{} Set custom data {} for {} with id {}",
          LOG_PREFIX,
          serverDataItem,
          entityDataAccessor,
          entityDataAccessor.getIndex());
      serverDataItem.setValue(customData);
    }
  }

  public <T> T get(ServerDataAccessor<T> entityDataAccessor) {
    ServerDataItem<T> serverDataItem = this.getDataItem(entityDataAccessor);
    if (serverDataItem != null) {
      return serverDataItem.getValue();
    }
    return null;
  }

  private <T> ServerDataItem<T> getDataItem(ServerDataAccessor<T> entityDataAccessor) {
    try {
      @SuppressWarnings("unchecked")
      ServerDataItem<T> serverDataItem =
          (ServerDataItem<T>) this.customEntityDataMap.get(entityDataAccessor.getIndex());
      return serverDataItem;
    } catch (Exception exception) {
      log.error(
          "{} Failed to get data item for {} with {}", LOG_PREFIX, entityDataAccessor, exception);
    }
    return null;
  }
}

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

package de.markusbordihn.easynpc.data.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.data.custom.CustomDataIndex;
import de.markusbordihn.easynpc.data.custom.CustomDataItem;
import java.util.EnumMap;
import java.util.HashSet;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomEntityData {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Custom Entity Data]";
  private static final HashSet<CustomDataIndex> usedCustomDataAccessorIdSet = new HashSet<>();
  private static int customDataAccessorId = -1;
  // Internal storage (using EnumMap for better performance)
  private final EnumMap<CustomDataIndex, CustomDataItem<?>> customEntityDataMap =
      new EnumMap<>(CustomDataIndex.class);
  // Cache
  private final boolean isClientSide;

  public CustomEntityData(Entity entity) {
    this.isClientSide = entity != null && entity.level != null && entity.level.isClientSide;
  }

  /*
   * Auto-generated id, should be mostly used for testing purpose. For production use, please define
   * a custom index instead!
   */
  public static <T> CustomDataAccessor<T> defineId(EntityDataSerializer<T> entityDataSerializers) {
    // Check if we have a free custom data accessor id.
    if (customDataAccessorId >= CustomDataIndex.MAX_FREE_INDEX) {
      log.error(
          "{} No more custom data accessor available for {} with id {}"
              + "The maximum for auto-generated ids is {}",
          LOG_PREFIX,
          entityDataSerializers,
          customDataAccessorId,
          CustomDataIndex.MAX_FREE_INDEX);
      log.error("{} Please define a custom index instead of using the auto-generated.", LOG_PREFIX);
      return null;
    }

    return defineId(CustomDataIndex.getIndex(++customDataAccessorId), entityDataSerializers);
  }

  public static <T> CustomDataAccessor<T> defineId(
      CustomDataIndex customDataIndex, EntityDataSerializer<T> entityDataSerializers) {

    // Make sure that we have a valid custom data accessor id and it is not already in use.
    if (usedCustomDataAccessorIdSet.contains(customDataIndex)) {
      log.error(
          "{} Can't define custom data accessor {} with id {}, because it is already in use!",
          LOG_PREFIX,
          entityDataSerializers,
          customDataIndex.ordinal());
      return null;
    }

    CustomDataAccessor<T> customDataAccessor =
        new CustomDataAccessor<>(customDataIndex, entityDataSerializers);
    log.debug(
        "{} Create custom data accessor {} with id {}",
        LOG_PREFIX,
        customDataAccessor,
        customDataIndex);
    usedCustomDataAccessorIdSet.add(customDataIndex);
    return customDataAccessor;
  }

  public <T> void define(CustomDataAccessor<T> customDataAccessor, T customData) {
    if (!this.isClientSide) {
      CustomDataItem<T> dataItem = new CustomDataItem<>(customDataAccessor, customData);
      log.debug("{} Define custom data item {} with {}", LOG_PREFIX, dataItem, customDataAccessor);
      this.customEntityDataMap.put(customDataAccessor.getIndex(), dataItem);
    }
  }

  public <T> void set(CustomDataAccessor<T> entityDataAccessor, T customData) {
    CustomDataItem<T> customDataItem = this.getDataItem(entityDataAccessor);
    if (customDataItem != null) {
      log.debug(
          "{} Set custom data {} for {} with id {}",
          LOG_PREFIX,
          customDataItem,
          entityDataAccessor,
          entityDataAccessor.getIndex());
      customDataItem.setValue(customData);
    }
  }

  public <T> T get(CustomDataAccessor<T> entityDataAccessor) {
    CustomDataItem<T> customDataItem = this.getDataItem(entityDataAccessor);
    if (customDataItem != null) {
      return customDataItem.getValue();
    }
    return null;
  }

  private <T> CustomDataItem<T> getDataItem(CustomDataAccessor<T> entityDataAccessor) {
    try {
      @SuppressWarnings("unchecked")
      CustomDataItem<T> customDataItem =
          (CustomDataItem<T>) this.customEntityDataMap.get(entityDataAccessor.getIndex());
      return customDataItem;
    } catch (Exception exception) {
      log.error(
          "{} Failed to get data item for {} with {}", LOG_PREFIX, entityDataAccessor, exception);
    }
    return null;
  }
}

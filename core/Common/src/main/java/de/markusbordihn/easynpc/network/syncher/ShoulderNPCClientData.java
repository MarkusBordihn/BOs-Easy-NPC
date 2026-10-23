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

package de.markusbordihn.easynpc.network.syncher;

import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;

public final class ShoulderNPCClientData {

  private static final String VANILLA_BRAIN_TAG = "Brain";
  private static final String VANILLA_GOSSIPS_TAG = "Gossips";
  private static final Set<String> SERVER_ONLY_TAGS =
      Set.of(
          ActionEventDataCapable.DATA_ACTION_DATA_TAG,
          DialogDataCapable.DATA_DIALOG_DATA_TAG,
          FactionDataCapable.DATA_FACTION_NAME_TAG,
          InventoryDataCapable.DATA_INVENTORY_TAG,
          NavigationDataCapable.DATA_NAVIGATION_TAG,
          ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG,
          PresetDataCapable.CUSTOM_IDENTIFIER_TAG,
          PresetDataCapable.PRESET_METADATA_TAG,
          PresetDataCapable.PRESET_UUID_TAG,
          PresetDataCapable.RESTORE_ON_OWNER_LOGIN_TAG,
          SoundDataCapable.EASY_NPC_DATA_SOUND_DATA_TAG,
          StatusDataCapable.DATA_STATUS_DATA_TAG,
          TradingDataCapable.DATA_TRADING_DATA_TAG,
          TradingDataCapable.DATA_OFFERS_TAG,
          VANILLA_BRAIN_TAG,
          VANILLA_GOSSIPS_TAG);

  private ShoulderNPCClientData() {}

  public static List<SynchedEntityData.DataValue<?>> createClientDataValues(
      Entity entity, List<SynchedEntityData.DataValue<?>> dataValues) {
    if (!(entity instanceof Player player) || dataValues == null) {
      return dataValues;
    }

    List<SynchedEntityData.DataValue<?>> clientDataValues = null;
    for (int i = 0; i < dataValues.size(); i++) {
      SynchedEntityData.DataValue<?> dataValue = dataValues.get(i);
      if (dataValue.serializer() != EntityDataSerializers.COMPOUND_TAG
          || !(dataValue.value() instanceof CompoundTag entityTag)
          || !EasyNPCEntityTypes.isEasyNPCEntityTag(entityTag, player.level())) {
        continue;
      }

      if (clientDataValues == null) {
        clientDataValues = new ArrayList<>(dataValues);
      }
      clientDataValues.set(
          i,
          new SynchedEntityData.DataValue<>(
              dataValue.id(), EntityDataSerializers.COMPOUND_TAG, createClientEntityTag(entityTag)));
    }
    return clientDataValues != null ? clientDataValues : dataValues;
  }

  public static CompoundTag createClientEntityTag(CompoundTag entityTag) {
    CompoundTag clientEntityTag = entityTag.copy();
    SERVER_ONLY_TAGS.forEach(clientEntityTag::remove);
    return clientEntityTag;
  }
}

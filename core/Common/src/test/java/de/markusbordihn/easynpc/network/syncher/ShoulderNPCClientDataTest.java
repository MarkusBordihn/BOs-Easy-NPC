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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;

import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import java.util.List;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ShoulderNPCClientDataTest {

  private static final String ENTITY_ID_TAG = "id";
  private static final String ENTITY_UUID_TAG = "UUID";

  private static CompoundTag createShoulderNPCTag(String... tagNames) {
    CompoundTag entityTag = new CompoundTag();
    entityTag.putString(ENTITY_ID_TAG, "easy_npc:allay");
    entityTag.putUUID(ENTITY_UUID_TAG, UUID.randomUUID());
    for (String tagName : tagNames) {
      CompoundTag dataTag = new CompoundTag();
      dataTag.putString("Value", tagName);
      entityTag.put(tagName, dataTag);
    }
    return entityTag;
  }

  @ParameterizedTest
  @ValueSource(
      strings = {
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
        TradingDataCapable.DATA_TRADING_INVENTORY_TAG,
        TradingDataCapable.DATA_TRADING_OFFERS_TAG,
        TradingDataCapable.DATA_TRADING_RECIPES_TAG,
        "Brain",
        "Gossips"
      })
  @DisplayName("Server-only NPC data is not sent to clients")
  void serverOnlyDataIsRemoved(String tagName) {
    assertFalse(
        ShoulderNPCClientData.createClientEntityTag(createShoulderNPCTag(tagName))
            .contains(tagName));
  }

  @ParameterizedTest
  @ValueSource(
      strings = {
        ENTITY_ID_TAG,
        ENTITY_UUID_TAG,
        DisplayAttributeDataCapable.DATA_DISPLAY_ATTRIBUTE_TAG,
        EntityAttributes.ENTITY_ATTRIBUTE_TAG,
        ModelDataCapable.EASY_NPC_DATA_MODEL_DATA_TAG,
        OwnerDataCapable.DATA_OWNER_TAG,
        RenderDataCapable.DATA_RENDER_DATA_TAG,
        SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG,
        "ArmorItems",
        "HandItems",
        "VillagerData"
      })
  @DisplayName("Data needed to render the NPC on a shoulder is kept")
  void renderDataIsKept(String tagName) {
    CompoundTag entityTag = createShoulderNPCTag(tagName);

    assertEquals(
        entityTag.get(tagName), ShoulderNPCClientData.createClientEntityTag(entityTag).get(tagName));
  }

  @Test
  @DisplayName("The server-side shoulder data stays complete")
  void sourceTagIsNotChanged() {
    CompoundTag entityTag =
        createShoulderNPCTag(
            DialogDataCapable.DATA_DIALOG_DATA_TAG, TradingDataCapable.DATA_TRADING_DATA_TAG);
    CompoundTag originalEntityTag = entityTag.copy();

    ShoulderNPCClientData.createClientEntityTag(entityTag);

    assertEquals(originalEntityTag, entityTag);
  }

  @Test
  @DisplayName("Data values of entities other than players are passed through unchanged")
  void nonPlayerDataValuesAreUnchanged() {
    List<SynchedEntityData.DataValue<?>> dataValues = List.of();

    assertSame(dataValues, ShoulderNPCClientData.createClientDataValues(null, dataValues));
  }
}

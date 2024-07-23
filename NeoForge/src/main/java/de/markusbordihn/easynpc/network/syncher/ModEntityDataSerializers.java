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

package de.markusbordihn.easynpc.network.syncher;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.trading.TradingType;
import java.util.HashSet;
import java.util.UUID;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;
import net.neoforged.neoforge.registries.NeoForgeRegistries;

public class ModEntityDataSerializers {

  public static final DeferredRegister<EntityDataSerializer<?>> ENTITY_DATA_SERIALIZERS =
      DeferredRegister.create(NeoForgeRegistries.ENTITY_DATA_SERIALIZERS, Constants.MOD_ID);

  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<ActionEventSet>>
      ACTION_EVENT_SET =
          ENTITY_DATA_SERIALIZERS.register(
              "action_event_set", () -> EntityDataSerializersManager.ACTION_EVENT_SET);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<DialogDataSet>>
      DIALOG_DATA_SET =
          ENTITY_DATA_SERIALIZERS.register(
              "dialog_data_set", () -> EntityDataSerializersManager.DIALOG_DATA_SET);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<MerchantOffers>>
      MERCHANT_OFFERS =
          ENTITY_DATA_SERIALIZERS.register(
              "merchant_offers", () -> EntityDataSerializersManager.MERCHANT_OFFERS);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<ModelPose>>
      MODEL_POSE =
          ENTITY_DATA_SERIALIZERS.register(
              "model_pose", () -> EntityDataSerializersManager.MODEL_POSE);
  public static final DeferredHolder<
          EntityDataSerializer<?>, EntityDataSerializer<ObjectiveDataSet>>
      OBJECTIVE_DATA_SET =
          ENTITY_DATA_SERIALIZERS.register(
              "objective_data_set", () -> EntityDataSerializersManager.OBJECTIVE_DATA_SET);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<CustomPosition>>
      POSITION =
          ENTITY_DATA_SERIALIZERS.register("position", () -> EntityDataSerializersManager.POSITION);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<Profession>>
      PROFESSION =
          ENTITY_DATA_SERIALIZERS.register(
              "profession", () -> EntityDataSerializersManager.PROFESSION);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<RenderDataSet>>
      RENDER_DATA_SET =
          ENTITY_DATA_SERIALIZERS.register(
              "render_data_set", () -> EntityDataSerializersManager.RENDER_DATA_SET);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<CustomRotation>>
      ROTATION =
          ENTITY_DATA_SERIALIZERS.register("rotation", () -> EntityDataSerializersManager.ROTATION);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<CustomScale>>
      SCALE = ENTITY_DATA_SERIALIZERS.register("scale", () -> EntityDataSerializersManager.SCALE);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<SkinType>>
      SKIN_TYPE =
          ENTITY_DATA_SERIALIZERS.register(
              "skin_type", () -> EntityDataSerializersManager.SKIN_TYPE);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<UUID>>
      SKIN_UUID =
          ENTITY_DATA_SERIALIZERS.register(
              "skin_uuid", () -> EntityDataSerializersManager.SKIN_UUID);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<SoundDataSet>>
      SOUND_DATA_SET =
          ENTITY_DATA_SERIALIZERS.register(
              "sound_data_set", () -> EntityDataSerializersManager.SOUND_DATA_SET);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<UUID>>
      SPAWNER_UUID =
          ENTITY_DATA_SERIALIZERS.register(
              "spawner_uuid", () -> EntityDataSerializersManager.SPAWNER_UUID);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<TradingType>>
      TRADING_TYPE =
          ENTITY_DATA_SERIALIZERS.register(
              "trading_type", () -> EntityDataSerializersManager.TRADING_TYPE);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<HashSet<UUID>>>
      TARGETED_ENTITY_HASH_SET =
          ENTITY_DATA_SERIALIZERS.register(
              "targeted_entity_hash_set",
              () -> EntityDataSerializersManager.TARGETED_ENTITY_HASH_SET);
  public static final DeferredHolder<EntityDataSerializer<?>, EntityDataSerializer<HashSet<String>>>
      TARGETED_PLAYER_HASH_SET =
          ENTITY_DATA_SERIALIZERS.register(
              "targeted_player_hash_set",
              () -> EntityDataSerializersManager.TARGETED_PLAYER_HASH_SET);

  private ModEntityDataSerializers() {}
}

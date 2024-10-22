/**
 * Copyright 2023 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.easynpc.network.syncher;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.attribute.CustomAttributes;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeSet;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.TargetedEntitySet;
import de.markusbordihn.easynpc.data.objective.TargetedPlayerSet;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.skin.SkinUUID;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.spawner.SpawnerUUID;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import de.markusbordihn.easynpc.data.trading.TradingType;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EntityDataSerializersManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<String, EntityDataSerializer<?>> ENTITY_DATA_SERIALIZERS =
      new LinkedHashMap<>();
  public static final EntityDataSerializer<ActionEventSet> ACTION_EVENT_SET =
      defineSerializer(
          ActionEventSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(ActionEventSet.STREAM_CODEC));
  public static final EntityDataSerializer<CustomAttributes> CUSTOM_ATTRIBUTES =
      defineSerializer(
          CustomAttributes.class.getSimpleName(),
          EntityDataSerializer.forValueType(CustomAttributes.STREAM_CODEC));
  public static final EntityDataSerializer<DialogDataSet> DIALOG_DATA_SET =
      defineSerializer(
          DialogDataSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(DialogDataSet.STREAM_CODEC));
  public static final EntityDataSerializer<DisplayAttributeSet> DISPLAY_ATTRIBUTE_SET =
      defineSerializer(
          DisplayAttributeSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(DisplayAttributeSet.STREAM_CODEC));
  public static final EntityDataSerializer<EntityAttributes> ENTITY_ATTRIBUTES =
      defineSerializer(
          EntityAttributes.class.getSimpleName(),
          EntityDataSerializer.forValueType(EntityAttributes.STREAM_CODEC));
  public static final EntityDataSerializer<MerchantOffers> MERCHANT_OFFERS =
      defineSerializer(
          MerchantOffers.class.getSimpleName(),
          EntityDataSerializer.forValueType(MerchantOffers.STREAM_CODEC));
  public static final EntityDataSerializer<ModelPose> MODEL_POSE =
      defineSerializer(
          ModelPose.class.getSimpleName(),
          EntityDataSerializer.forValueType(ModelPose.STREAM_CODEC));
  public static final EntityDataSerializer<ObjectiveDataSet> OBJECTIVE_DATA_SET =
      defineSerializer(
          ObjectiveDataSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(ObjectiveDataSet.STREAM_CODEC));
  public static final EntityDataSerializer<CustomPosition> POSITION =
      defineSerializer(
          CustomPosition.class.getSimpleName(),
          EntityDataSerializer.forValueType(CustomPosition.STREAM_CODEC));
  public static final EntityDataSerializer<Profession> PROFESSION =
      defineSerializer(
          Profession.class.getSimpleName(),
          EntityDataSerializer.forValueType(Profession.STREAM_CODEC));
  public static final EntityDataSerializer<RenderDataSet> RENDER_DATA_SET =
      defineSerializer(
          RenderDataSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(RenderDataSet.STREAM_CODEC));
  public static final EntityDataSerializer<CustomRotation> ROTATION =
      defineSerializer(
          CustomRotation.class.getSimpleName(),
          EntityDataSerializer.forValueType(CustomRotation.STREAM_CODEC));
  public static final EntityDataSerializer<CustomScale> SCALE =
      defineSerializer(
          CustomScale.class.getSimpleName(),
          EntityDataSerializer.forValueType(CustomScale.STREAM_CODEC));
  public static final EntityDataSerializer<SkinDataEntry> SKIN_DATA_ENTRY =
      defineSerializer(
          SkinDataEntry.class.getSimpleName(),
          EntityDataSerializer.forValueType(SkinDataEntry.STREAM_CODEC));
  public static final EntityDataSerializer<SkinType> SKIN_TYPE =
      defineSerializer(
          SkinType.class.getSimpleName(), EntityDataSerializer.forValueType(SkinType.STREAM_CODEC));
  public static final EntityDataSerializer<UUID> SKIN_UUID =
      defineSerializer(
          SkinUUID.class.getSimpleName(), EntityDataSerializer.forValueType(SkinUUID.STREAM_CODEC));
  public static final EntityDataSerializer<SoundDataSet> SOUND_DATA_SET =
      defineSerializer(
          SoundDataSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(SoundDataSet.STREAM_CODEC));
  public static final EntityDataSerializer<UUID> SPAWNER_UUID =
      defineSerializer(
          SpawnerUUID.class.getSimpleName(),
          EntityDataSerializer.forValueType(SpawnerUUID.STREAM_CODEC));
  public static final EntityDataSerializer<HashSet<UUID>> TARGETED_ENTITY_HASH_SET =
      defineSerializer(
          TargetedEntitySet.class.getSimpleName(),
          EntityDataSerializer.forValueType(TargetedEntitySet.STREAM_CODEC));
  public static final EntityDataSerializer<HashSet<String>> TARGETED_PLAYER_HASH_SET =
      defineSerializer(
          TargetedPlayerSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(TargetedPlayerSet.STREAM_CODEC));
  public static final EntityDataSerializer<TradingDataSet> TRADING_DATA_SET =
      defineSerializer(
          TradingDataSet.class.getSimpleName(),
          EntityDataSerializer.forValueType(TradingDataSet.STREAM_CODEC));
  public static final EntityDataSerializer<TradingType> TRADING_TYPE =
      defineSerializer(
          TradingType.class.getSimpleName(),
          EntityDataSerializer.forValueType(TradingType.STREAM_CODEC));

  private EntityDataSerializersManager() {}

  public static <T> EntityDataSerializer<T> defineSerializer(
      final String className, final EntityDataSerializer<T> serializer) {
    if (ENTITY_DATA_SERIALIZERS.containsKey(className)) {
      log.error(
          "Entity data serializer {} already defined with {}!",
          className,
          ENTITY_DATA_SERIALIZERS.get(className));
      return null;
    }
    ENTITY_DATA_SERIALIZERS.put(className, serializer);
    return serializer;
  }

  public static void register() {
    for (Map.Entry<String, EntityDataSerializer<?>> entry : ENTITY_DATA_SERIALIZERS.entrySet()) {
      EntityDataSerializer<?> serializer = entry.getValue();
      if (serializer == null) {
        log.error("Failed to register entity data serializer {}", entry.getKey());
        continue;
      }
      EntityDataSerializers.registerSerializer(serializer);
      int id = EntityDataSerializers.getSerializedId(serializer);
      if (id >= 0) {
        log.info("Registered entity data serializer {} with id {}", entry.getKey(), id);
      } else {
        log.error(
            "Failed to register entity data serializer {} with {}", entry.getKey(), serializer);
      }
    }
  }
}

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
import java.util.UUID;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EntityDataSerializersManager {

  public static final EntityDataSerializer<ActionEventSet> ACTION_EVENT_SET =
      EntityDataSerializer.forValueType(ActionEventSet.STREAM_CODEC);
  public static final EntityDataSerializer<DialogDataSet> DIALOG_DATA_SET =
      EntityDataSerializer.forValueType(DialogDataSet.STREAM_CODEC);
  public static final EntityDataSerializer<DisplayAttributeSet> DISPLAY_ATTRIBUTE_SET =
      EntityDataSerializer.forValueType(DisplayAttributeSet.STREAM_CODEC);
  public static final EntityDataSerializer<MerchantOffers> MERCHANT_OFFERS =
      EntityDataSerializer.forValueType(MerchantOffers.STREAM_CODEC);
  public static final EntityDataSerializer<ModelPose> MODEL_POSE =
      EntityDataSerializer.forValueType(ModelPose.STREAM_CODEC);
  public static final EntityDataSerializer<ObjectiveDataSet> OBJECTIVE_DATA_SET =
      EntityDataSerializer.forValueType(ObjectiveDataSet.STREAM_CODEC);
  public static final EntityDataSerializer<CustomPosition> POSITION =
      EntityDataSerializer.forValueType(CustomPosition.STREAM_CODEC);
  public static final EntityDataSerializer<Profession> PROFESSION =
      EntityDataSerializer.forValueType(Profession.STREAM_CODEC);
  public static final EntityDataSerializer<RenderDataSet> RENDER_DATA_SET =
      EntityDataSerializer.forValueType(RenderDataSet.STREAM_CODEC);
  public static final EntityDataSerializer<CustomRotation> ROTATION =
      EntityDataSerializer.forValueType(CustomRotation.STREAM_CODEC);
  public static final EntityDataSerializer<CustomScale> SCALE =
      EntityDataSerializer.forValueType(CustomScale.STREAM_CODEC);
  public static final EntityDataSerializer<SkinDataEntry> SKIN_DATA_ENTRY =
      EntityDataSerializer.forValueType(SkinDataEntry.STREAM_CODEC);
  public static final EntityDataSerializer<SkinType> SKIN_TYPE =
      EntityDataSerializer.forValueType(SkinType.STREAM_CODEC);
  public static final EntityDataSerializer<UUID> SKIN_UUID =
      EntityDataSerializer.forValueType(SkinUUID.STREAM_CODEC);
  public static final EntityDataSerializer<SoundDataSet> SOUND_DATA_SET =
      EntityDataSerializer.forValueType(SoundDataSet.STREAM_CODEC);
  public static final EntityDataSerializer<UUID> SPAWNER_UUID =
      EntityDataSerializer.forValueType(SpawnerUUID.STREAM_CODEC);
  public static final EntityDataSerializer<HashSet<UUID>> TARGETED_ENTITY_HASH_SET =
      EntityDataSerializer.forValueType(TargetedEntitySet.STREAM_CODEC);
  public static final EntityDataSerializer<HashSet<String>> TARGETED_PLAYER_HASH_SET =
      EntityDataSerializer.forValueType(TargetedPlayerSet.STREAM_CODEC);
  public static final EntityDataSerializer<TradingDataSet> TRADING_DATA_SET =
      EntityDataSerializer.forValueType(TradingDataSet.STREAM_CODEC);
  public static final EntityDataSerializer<TradingType> TRADING_TYPE =
      EntityDataSerializer.forValueType(TradingType.STREAM_CODEC);

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EntityDataSerializersManager() {}

  public static void register() {
    log.info("{} Entity Data Serializers ...", Constants.LOG_REGISTER_PREFIX);
    EntityDataSerializers.registerSerializer(ACTION_EVENT_SET);
    EntityDataSerializers.registerSerializer(DIALOG_DATA_SET);
    EntityDataSerializers.registerSerializer(DISPLAY_ATTRIBUTE_SET);
    EntityDataSerializers.registerSerializer(MERCHANT_OFFERS);
    EntityDataSerializers.registerSerializer(MODEL_POSE);
    EntityDataSerializers.registerSerializer(OBJECTIVE_DATA_SET);
    EntityDataSerializers.registerSerializer(POSITION);
    EntityDataSerializers.registerSerializer(PROFESSION);
    EntityDataSerializers.registerSerializer(RENDER_DATA_SET);
    EntityDataSerializers.registerSerializer(ROTATION);
    EntityDataSerializers.registerSerializer(SCALE);
    EntityDataSerializers.registerSerializer(SKIN_DATA_ENTRY);
    EntityDataSerializers.registerSerializer(SKIN_TYPE);
    EntityDataSerializers.registerSerializer(SKIN_UUID);
    EntityDataSerializers.registerSerializer(SOUND_DATA_SET);
    EntityDataSerializers.registerSerializer(SPAWNER_UUID);
    EntityDataSerializers.registerSerializer(TARGETED_ENTITY_HASH_SET);
    EntityDataSerializers.registerSerializer(TARGETED_PLAYER_HASH_SET);
    EntityDataSerializers.registerSerializer(TRADING_DATA_SET);
    EntityDataSerializers.registerSerializer(TRADING_TYPE);
  }
}

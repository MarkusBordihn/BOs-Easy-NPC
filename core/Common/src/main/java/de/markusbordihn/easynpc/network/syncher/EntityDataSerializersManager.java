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
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeDataSet;
import de.markusbordihn.easynpc.data.model.ModelAnimationData;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
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
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, ActionEventSet value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public ActionEventSet read(FriendlyByteBuf buffer) {
              return new ActionEventSet(buffer.readNbt());
            }

            @Override
            public ActionEventSet copy(ActionEventSet value) {
              return value;
            }
          });
  public static final EntityDataSerializer<DialogDataSet> DIALOG_DATA_SET =
      defineSerializer(
          DialogDataSet.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, DialogDataSet value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public DialogDataSet read(FriendlyByteBuf buffer) {
              return new DialogDataSet(buffer.readNbt());
            }

            @Override
            public DialogDataSet copy(DialogDataSet value) {
              return value;
            }
          });
  public static final EntityDataSerializer<DisplayAttributeDataSet> DISPLAY_ATTRIBUTE =
      defineSerializer(
          DisplayAttributeDataSet.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, DisplayAttributeDataSet value) {
              value.encode(buffer);
            }

            @Override
            public DisplayAttributeDataSet read(FriendlyByteBuf buffer) {
              return DisplayAttributeDataSet.decode(buffer);
            }

            @Override
            public DisplayAttributeDataSet copy(DisplayAttributeDataSet value) {
              return value;
            }
          });
  public static final EntityDataSerializer<EntityAttributes> ENTITY_ATTRIBUTES =
      defineSerializer(
          EntityAttributes.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, EntityAttributes value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public EntityAttributes read(FriendlyByteBuf buffer) {
              return new EntityAttributes(buffer.readNbt());
            }

            @Override
            public EntityAttributes copy(EntityAttributes value) {
              return value;
            }
          });
  public static final EntityDataSerializer<MerchantOffers> MERCHANT_OFFERS =
      defineSerializer(
          MerchantOffers.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, MerchantOffers value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public MerchantOffers read(FriendlyByteBuf buffer) {
              CompoundTag compoundTag = buffer.readNbt();
              return compoundTag != null ? new MerchantOffers(compoundTag) : new MerchantOffers();
            }

            @Override
            public MerchantOffers copy(MerchantOffers value) {
              return value;
            }
          });
  public static final EntityDataSerializer<ModelPose> MODEL_POSE =
      defineSerializer(
          ModelPose.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, ModelPose modelPose) {
              buffer.writeEnum(modelPose);
            }

            @Override
            public ModelPose read(FriendlyByteBuf buffer) {
              return buffer.readEnum(ModelPose.class);
            }

            @Override
            public ModelPose copy(ModelPose value) {
              return value;
            }
          });
  public static final EntityDataSerializer<ModelAnimationData> MODEL_ANIMATION_DATA =
      defineSerializer(
          ModelAnimationData.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, ModelAnimationData animationData) {
              animationData.encode(buffer);
            }

            @Override
            public ModelAnimationData read(FriendlyByteBuf buffer) {
              return ModelAnimationData.decode(buffer);
            }

            @Override
            public ModelAnimationData copy(ModelAnimationData value) {
              return value;
            }
          });
  public static final EntityDataSerializer<ObjectiveDataSet> OBJECTIVE_DATA_SET =
      defineSerializer(
          ObjectiveDataSet.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, ObjectiveDataSet value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public ObjectiveDataSet read(FriendlyByteBuf buffer) {
              return new ObjectiveDataSet(buffer.readNbt());
            }

            @Override
            public ObjectiveDataSet copy(ObjectiveDataSet value) {
              return value;
            }
          });
  public static final EntityDataSerializer<Profession> PROFESSION =
      defineSerializer(
          Profession.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, Profession value) {
              buffer.writeEnum(value);
            }

            @Override
            public Profession read(FriendlyByteBuf buffer) {
              return buffer.readEnum(Profession.class);
            }

            @Override
            public Profession copy(Profession value) {
              return value;
            }
          });
  public static final EntityDataSerializer<RenderDataEntry> RENDER_DATA_SET =
      defineSerializer(
          RenderDataEntry.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, RenderDataEntry value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public RenderDataEntry read(FriendlyByteBuf buffer) {
              return new RenderDataEntry(buffer.readNbt());
            }

            @Override
            public RenderDataEntry copy(RenderDataEntry value) {
              return value;
            }
          });
  public static final EntityDataSerializer<Map<ModelPartType, CustomRotation>> MODEL_PART_ROTATION =
      defineSerializer(
          ModelPartType.class.getSimpleName() + ":CustomRotation",
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, Map<ModelPartType, CustomRotation> value) {
              buffer.writeVarInt(value.size());
              for (Map.Entry<ModelPartType, CustomRotation> entry : value.entrySet()) {
                buffer.writeEnum(entry.getKey());
                entry.getValue().encode(buffer);
              }
            }

            @Override
            public Map<ModelPartType, CustomRotation> read(FriendlyByteBuf buffer) {
              int size = buffer.readVarInt();
              Map<ModelPartType, CustomRotation> value = new EnumMap<>(ModelPartType.class);
              for (int i = 0; i < size; i++) {
                value.put(buffer.readEnum(ModelPartType.class), CustomRotation.decode(buffer));
              }
              return value;
            }

            @Override
            public Map<ModelPartType, CustomRotation> copy(
                Map<ModelPartType, CustomRotation> value) {
              return new EnumMap<>(value);
            }
          });
  public static final EntityDataSerializer<Map<ModelPartType, CustomPosition>> MODEL_PART_POSITION =
      defineSerializer(
          ModelPartType.class.getSimpleName() + ":CustomPosition",
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, Map<ModelPartType, CustomPosition> value) {
              buffer.writeVarInt(value.size());
              for (Map.Entry<ModelPartType, CustomPosition> entry : value.entrySet()) {
                buffer.writeEnum(entry.getKey());
                entry.getValue().encode(buffer);
              }
            }

            @Override
            public Map<ModelPartType, CustomPosition> read(FriendlyByteBuf buffer) {
              int size = buffer.readVarInt();
              Map<ModelPartType, CustomPosition> value = new EnumMap<>(ModelPartType.class);
              for (int i = 0; i < size; i++) {
                value.put(buffer.readEnum(ModelPartType.class), CustomPosition.decode(buffer));
              }
              return value;
            }

            @Override
            public Map<ModelPartType, CustomPosition> copy(
                Map<ModelPartType, CustomPosition> value) {
              return new EnumMap<>(value);
            }
          });
  public static final EntityDataSerializer<Map<ModelPartType, CustomScale>> MODEL_PART_SCALE =
      defineSerializer(
          ModelPartType.class.getSimpleName() + ":CustomScale",
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, Map<ModelPartType, CustomScale> value) {
              buffer.writeVarInt(value.size());
              for (Map.Entry<ModelPartType, CustomScale> entry : value.entrySet()) {
                buffer.writeEnum(entry.getKey());
                entry.getValue().encode(buffer);
              }
            }

            @Override
            public Map<ModelPartType, CustomScale> read(FriendlyByteBuf buffer) {
              int size = buffer.readVarInt();
              Map<ModelPartType, CustomScale> value = new EnumMap<>(ModelPartType.class);
              for (int i = 0; i < size; i++) {
                value.put(buffer.readEnum(ModelPartType.class), CustomScale.decode(buffer));
              }
              return value;
            }

            @Override
            public Map<ModelPartType, CustomScale> copy(Map<ModelPartType, CustomScale> value) {
              return new EnumMap<>(value);
            }
          });
  public static final EntityDataSerializer<Map<ModelPartType, Boolean>> MODEL_PART_VISIBILITY =
      defineSerializer(
          ModelPartType.class.getSimpleName() + ":Visibility",
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, Map<ModelPartType, Boolean> value) {
              buffer.writeVarInt(value.size());
              for (Map.Entry<ModelPartType, Boolean> entry : value.entrySet()) {
                buffer.writeEnum(entry.getKey());
                buffer.writeBoolean(entry.getValue());
              }
            }

            @Override
            public Map<ModelPartType, Boolean> read(FriendlyByteBuf buffer) {
              int size = buffer.readVarInt();
              Map<ModelPartType, Boolean> value = new EnumMap<>(ModelPartType.class);
              for (int i = 0; i < size; i++) {
                value.put(buffer.readEnum(ModelPartType.class), buffer.readBoolean());
              }
              return value;
            }

            @Override
            public Map<ModelPartType, Boolean> copy(Map<ModelPartType, Boolean> value) {
              return new EnumMap<>(value);
            }
          });
  public static final EntityDataSerializer<SkinDataEntry> SKIN_DATA_ENTRY =
      defineSerializer(
          SkinDataEntry.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, SkinDataEntry value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public SkinDataEntry read(FriendlyByteBuf buffer) {
              return new SkinDataEntry(buffer.readNbt());
            }

            @Override
            public SkinDataEntry copy(SkinDataEntry value) {
              return value;
            }
          });
  public static final EntityDataSerializer<SoundDataSet> SOUND_DATA_SET =
      defineSerializer(
          SoundDataSet.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, SoundDataSet value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public SoundDataSet read(FriendlyByteBuf buffer) {
              return new SoundDataSet(buffer.readNbt());
            }

            @Override
            public SoundDataSet copy(SoundDataSet value) {
              return value;
            }
          });
  public static final EntityDataSerializer<HashSet<UUID>> TARGETED_ENTITY_HASH_SET =
      defineSerializer(
          HashSet.class.getSimpleName() + ":" + UUID.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, HashSet<UUID> value) {
              for (UUID entry : value) {
                buffer.writeUUID(entry);
              }
            }

            @Override
            public HashSet<UUID> read(FriendlyByteBuf buffer) {
              HashSet<UUID> value = new HashSet<>();
              while (buffer.isReadable()) {
                value.add(buffer.readUUID());
              }
              return value;
            }

            @Override
            public HashSet<UUID> copy(HashSet<UUID> value) {
              return value;
            }
          });
  public static final EntityDataSerializer<HashSet<String>> TARGETED_PLAYER_HASH_SET =
      defineSerializer(
          HashSet.class.getSimpleName() + ":" + String.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, HashSet<String> value) {
              for (String entry : value) {
                buffer.writeUtf(entry);
              }
            }

            @Override
            public HashSet<String> read(FriendlyByteBuf buffer) {
              HashSet<String> value = new HashSet<>();
              while (buffer.isReadable()) {
                value.add(buffer.readUtf());
              }
              return value;
            }

            @Override
            public HashSet<String> copy(HashSet<String> value) {
              return value;
            }
          });
  public static final EntityDataSerializer<TradingDataSet> TRADING_DATA_SET =
      defineSerializer(
          TradingDataSet.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, TradingDataSet value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public TradingDataSet read(FriendlyByteBuf buffer) {
              return new TradingDataSet(buffer.readNbt());
            }

            @Override
            public TradingDataSet copy(TradingDataSet value) {
              return value;
            }
          });
  public static final EntityDataSerializer<UUID> UUID =
      defineSerializer(
          UUID.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, UUID value) {
              buffer.writeUUID(value);
            }

            @Override
            public UUID read(FriendlyByteBuf buffer) {
              return buffer.readUUID();
            }

            @Override
            public UUID copy(UUID value) {
              return value;
            }
          });

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

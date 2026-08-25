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
import de.markusbordihn.easynpc.data.action.PendingActionSet;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeDataSet;
import de.markusbordihn.easynpc.data.model.ModelAnimationData;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.model.RootModelData;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.progression.ProgressionData;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.state.StateDataSet;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Function;
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
  public static final EntityDataSerializer<DisplayAttributeDataSet> DISPLAY_ATTRIBUTE =
      bufferSerializer(
          DisplayAttributeDataSet.class,
          DisplayAttributeDataSet::encode,
          DisplayAttributeDataSet::decode);
  public static final EntityDataSerializer<ModelPose> MODEL_POSE = enumSerializer(ModelPose.class);
  public static final EntityDataSerializer<ModelAnimationData> MODEL_ANIMATION_DATA =
      bufferSerializer(
          ModelAnimationData.class, ModelAnimationData::encode, ModelAnimationData::decode);
  public static final EntityDataSerializer<RootModelData> ROOT_MODEL_DATA =
      bufferSerializer(RootModelData.class, RootModelData::encode, RootModelData::decode);
  public static final EntityDataSerializer<Profession> PROFESSION =
      enumSerializer(Profession.class);
  public static final EntityDataSerializer<Map<ModelPartType, CustomRotation>> MODEL_PART_ROTATION =
      enumMapSerializer(
          ModelPartType.class, "CustomRotation", CustomRotation::encode, CustomRotation::decode);
  public static final EntityDataSerializer<Map<ModelPartType, CustomPosition>> MODEL_PART_POSITION =
      enumMapSerializer(
          ModelPartType.class, "CustomPosition", CustomPosition::encode, CustomPosition::decode);
  public static final EntityDataSerializer<Map<ModelPartType, CustomScale>> MODEL_PART_SCALE =
      enumMapSerializer(
          ModelPartType.class, "CustomScale", CustomScale::encode, CustomScale::decode);
  public static final EntityDataSerializer<Map<ModelPartType, Boolean>> MODEL_PART_VISIBILITY =
      enumMapSerializer(
          ModelPartType.class,
          "Visibility",
          (visible, buffer) -> buffer.writeBoolean(visible),
          FriendlyByteBuf::readBoolean);
  public static final EntityDataSerializer<HashSet<UUID>> TARGETED_ENTITY_HASH_SET =
      hashSetSerializer(
          UUID.class, (uuid, buffer) -> buffer.writeUUID(uuid), FriendlyByteBuf::readUUID);
  public static final EntityDataSerializer<HashSet<String>> TARGETED_PLAYER_HASH_SET =
      hashSetSerializer(
          String.class, (name, buffer) -> buffer.writeUtf(name), FriendlyByteBuf::readUtf);
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
  public static final EntityDataSerializer<ActionEventSet> ACTION_EVENT_SET =
      nbtSerializer(ActionEventSet.class, ActionEventSet::createTag, ActionEventSet::new);
  public static final EntityDataSerializer<PendingActionSet> PENDING_ACTION_SET =
      nbtSerializer(PendingActionSet.class, PendingActionSet::createTag, PendingActionSet::new);
  public static final EntityDataSerializer<StateDataSet> STATE_DATA_SET =
      nbtSerializer(StateDataSet.class, StateDataSet::createTag, StateDataSet::new);
  public static final EntityDataSerializer<DialogDataSet> DIALOG_DATA_SET =
      nbtSerializer(DialogDataSet.class, DialogDataSet::createTag, DialogDataSet::new);
  public static final EntityDataSerializer<EntityAttributes> ENTITY_ATTRIBUTES =
      nbtSerializer(EntityAttributes.class, EntityAttributes::createTag, EntityAttributes::new);
  public static final EntityDataSerializer<ProgressionData> PROGRESSION =
      nbtSerializer(
          ProgressionData.class,
          value -> value.encode(new CompoundTag()),
          compoundTag -> {
            if (compoundTag == null) {
              return new ProgressionData();
            }

            return ProgressionData.decode(compoundTag);
          });
  public static final EntityDataSerializer<MerchantOffers> MERCHANT_OFFERS =
      nbtSerializer(
          MerchantOffers.class,
          MerchantOffers::createTag,
          compoundTag -> {
            if (compoundTag == null) {
              return new MerchantOffers();
            }

            return new MerchantOffers(compoundTag);
          });
  public static final EntityDataSerializer<ObjectiveDataSet> OBJECTIVE_DATA_SET =
      nbtSerializer(ObjectiveDataSet.class, ObjectiveDataSet::createTag, ObjectiveDataSet::new);
  public static final EntityDataSerializer<RenderDataEntry> RENDER_DATA_SET =
      nbtSerializer(RenderDataEntry.class, RenderDataEntry::createTag, RenderDataEntry::new);
  public static final EntityDataSerializer<SkinDataEntry> SKIN_DATA_ENTRY =
      nbtSerializer(SkinDataEntry.class, SkinDataEntry::createTag, SkinDataEntry::new);
  public static final EntityDataSerializer<SoundDataSet> SOUND_DATA_SET =
      nbtSerializer(SoundDataSet.class, SoundDataSet::createTag, SoundDataSet::new);
  public static final EntityDataSerializer<TradingDataSet> TRADING_DATA_SET =
      nbtSerializer(TradingDataSet.class, TradingDataSet::createTag, TradingDataSet::new);
  private static final int RECOMMENDED_NBT_SIZE_BYTES = 8192; // 8 KB recommended
  private static final int WARNING_NBT_SIZE_BYTES = 32768; // 32 KB warning
  private static final int MAX_NBT_SIZE_BYTES = 2097152; // 2 MB absolute max

  private EntityDataSerializersManager() {}

  private static <T> EntityDataSerializer<T> bufferSerializer(
      final Class<T> dataClass,
      final BiConsumer<T, FriendlyByteBuf> encoder,
      final Function<FriendlyByteBuf, T> decoder) {
    return defineSerializer(
        dataClass.getSimpleName(),
        new EntityDataSerializer<>() {
          @Override
          public void write(FriendlyByteBuf buffer, T value) {
            encoder.accept(value, buffer);
          }

          @Override
          public T read(FriendlyByteBuf buffer) {
            return decoder.apply(buffer);
          }

          @Override
          public T copy(T value) {
            return value;
          }
        });
  }

  private static <T extends Enum<T>> EntityDataSerializer<T> enumSerializer(
      final Class<T> enumClass) {
    return defineSerializer(
        enumClass.getSimpleName(),
        new EntityDataSerializer<>() {
          @Override
          public void write(FriendlyByteBuf buffer, T value) {
            buffer.writeEnum(value);
          }

          @Override
          public T read(FriendlyByteBuf buffer) {
            return buffer.readEnum(enumClass);
          }

          @Override
          public T copy(T value) {
            return value;
          }
        });
  }

  private static <T> EntityDataSerializer<T> nbtSerializer(
      final Class<T> dataClass,
      final Function<T, CompoundTag> encoder,
      final Function<CompoundTag, T> decoder) {
    final String dataType = dataClass.getSimpleName();
    return defineSerializer(
        dataType,
        new EntityDataSerializer<>() {
          @Override
          public void write(FriendlyByteBuf buffer, T value) {
            buffer.writeNbt(validateAndGetNbt(encoder.apply(value), dataType));
          }

          @Override
          public T read(FriendlyByteBuf buffer) {
            return decoder.apply(buffer.readNbt());
          }

          @Override
          public T copy(T value) {
            return value;
          }
        });
  }

  private static <K extends Enum<K>, V> EntityDataSerializer<Map<K, V>> enumMapSerializer(
      final Class<K> keyClass,
      final String valueName,
      final BiConsumer<V, FriendlyByteBuf> encoder,
      final Function<FriendlyByteBuf, V> decoder) {
    return defineSerializer(
        keyClass.getSimpleName() + ":" + valueName,
        new EntityDataSerializer<>() {
          @Override
          public void write(FriendlyByteBuf buffer, Map<K, V> value) {
            buffer.writeVarInt(value.size());
            for (Map.Entry<K, V> entry : value.entrySet()) {
              buffer.writeEnum(entry.getKey());
              encoder.accept(entry.getValue(), buffer);
            }
          }

          @Override
          public Map<K, V> read(FriendlyByteBuf buffer) {
            int size = buffer.readVarInt();
            Map<K, V> value = new EnumMap<>(keyClass);
            for (int i = 0; i < size; i++) {
              value.put(buffer.readEnum(keyClass), decoder.apply(buffer));
            }
            return value;
          }

          @Override
          public Map<K, V> copy(Map<K, V> value) {
            return new EnumMap<>(value);
          }
        });
  }

  private static <T> EntityDataSerializer<HashSet<T>> hashSetSerializer(
      final Class<T> elementClass,
      final BiConsumer<T, FriendlyByteBuf> encoder,
      final Function<FriendlyByteBuf, T> decoder) {
    return defineSerializer(
        HashSet.class.getSimpleName() + ":" + elementClass.getSimpleName(),
        new EntityDataSerializer<>() {
          @Override
          public void write(FriendlyByteBuf buffer, HashSet<T> value) {
            buffer.writeVarInt(value.size());
            for (T entry : value) {
              encoder.accept(entry, buffer);
            }
          }

          @Override
          public HashSet<T> read(FriendlyByteBuf buffer) {
            int size = buffer.readVarInt();
            HashSet<T> value = new HashSet<>();
            for (int i = 0; i < size; i++) {
              value.add(decoder.apply(buffer));
            }
            return value;
          }

          @Override
          public HashSet<T> copy(HashSet<T> value) {
            return value;
          }
        });
  }

  private static CompoundTag validateAndGetNbt(CompoundTag tag, String dataType) {
    if (tag == null || !log.isDebugEnabled()) {
      return tag;
    }

    try {
      ByteBuf tempBuf = Unpooled.buffer();
      try {
        FriendlyByteBuf tempBuffer = new FriendlyByteBuf(tempBuf);
        tempBuffer.writeNbt(tag);
        int sizeBytes = tempBuffer.writerIndex();

        if (sizeBytes > MAX_NBT_SIZE_BYTES) {
          log.error(
              "[Entity Data] CRITICAL: {} NBT data size ({} bytes) exceeds maximum packet size! "
                  + "This WILL cause network errors and client crashes. "
                  + "Please reduce the amount of data stored in this field.",
              dataType,
              sizeBytes);
        } else if (sizeBytes > WARNING_NBT_SIZE_BYTES) {
          log.warn(
              "[Entity Data] {} NBT data size ({} bytes) is very large and may cause network issues. "
                  + "Recommended maximum is {} bytes. Consider reducing data amount.",
              dataType,
              sizeBytes,
              RECOMMENDED_NBT_SIZE_BYTES);
        } else if (sizeBytes > RECOMMENDED_NBT_SIZE_BYTES && log.isDebugEnabled()) {
          log.debug(
              "[Entity Data] {} NBT data size ({} bytes) exceeds recommended size of {} bytes.",
              dataType,
              sizeBytes,
              RECOMMENDED_NBT_SIZE_BYTES);
        }
      } finally {
        tempBuf.release();
      }
    } catch (Exception e) {
      if (log.isErrorEnabled()) {
        log.error("[Entity Data] Failed to validate NBT size for {}", dataType, e);
      }
    }
    return tag;
  }

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
        log.debug("Registered entity data serializer {} with id {}", entry.getKey(), id);
      } else {
        log.error(
            "Failed to register entity data serializer {} with {}", entry.getKey(), serializer);
      }
    }
  }
}

/*
 * Copyright 2024 Markus Bordihn
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
import de.markusbordihn.easynpc.data.attribute.CustomAttributes;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeSet;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
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
  public static final EntityDataSerializer<CustomAttributes> CUSTOM_ATTRIBUTES =
      defineSerializer(
          CustomAttributes.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, CustomAttributes value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public CustomAttributes read(FriendlyByteBuf buffer) {
              return new CustomAttributes(buffer.readNbt());
            }

            @Override
            public CustomAttributes copy(CustomAttributes value) {
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
  public static final EntityDataSerializer<DisplayAttributeSet> DISPLAY_ATTRIBUTE_SET =
      defineSerializer(
          DisplayAttributeSet.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, DisplayAttributeSet value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public DisplayAttributeSet read(FriendlyByteBuf buffer) {
              return new DisplayAttributeSet(buffer.readNbt());
            }

            @Override
            public DisplayAttributeSet copy(DisplayAttributeSet value) {
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
  public static final EntityDataSerializer<CustomPosition> POSITION =
      defineSerializer(
          CustomPosition.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, CustomPosition position) {
              buffer.writeFloat(position.x());
              buffer.writeFloat(position.y());
              buffer.writeFloat(position.z());
            }

            @Override
            public CustomPosition read(FriendlyByteBuf buffer) {
              return new CustomPosition(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
            }

            @Override
            public CustomPosition copy(CustomPosition position) {
              return position;
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
  public static final EntityDataSerializer<RenderDataSet> RENDER_DATA_SET =
      defineSerializer(
          RenderDataSet.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, RenderDataSet value) {
              buffer.writeNbt(value.createTag());
            }

            @Override
            public RenderDataSet read(FriendlyByteBuf buffer) {
              return new RenderDataSet(buffer.readNbt());
            }

            @Override
            public RenderDataSet copy(RenderDataSet value) {
              return value;
            }
          });
  public static final EntityDataSerializer<CustomRotation> ROTATION =
      defineSerializer(
          CustomRotation.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, CustomRotation rotation) {
              buffer.writeFloat(rotation.x());
              buffer.writeFloat(rotation.y());
              buffer.writeFloat(rotation.z());
            }

            @Override
            public CustomRotation read(FriendlyByteBuf buffer) {
              return new CustomRotation(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
            }

            @Override
            public CustomRotation copy(CustomRotation rotation) {
              return rotation;
            }
          });
  public static final EntityDataSerializer<CustomScale> SCALE =
      defineSerializer(
          CustomScale.class.getSimpleName(),
          new EntityDataSerializer<>() {
            @Override
            public void write(FriendlyByteBuf buffer, CustomScale scale) {
              buffer.writeFloat(scale.x());
              buffer.writeFloat(scale.y());
              buffer.writeFloat(scale.z());
            }

            @Override
            public CustomScale read(FriendlyByteBuf buffer) {
              return new CustomScale(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
            }

            @Override
            public CustomScale copy(CustomScale scale) {
              return scale;
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

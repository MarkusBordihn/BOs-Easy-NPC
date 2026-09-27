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

package de.markusbordihn.easynpc.data.preset;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public record PresetMetadata(
    String name,
    String category,
    String version,
    String author,
    long created,
    long modified,
    String description,
    String entityTypeId,
    String variantType,
    PresetAccess access) {

  public static final String TAG_NAME = "name";
  public static final String TAG_CATEGORY = "category";
  public static final String TAG_VERSION = "version";
  public static final String TAG_AUTHOR = "author";
  public static final String TAG_CREATED = "created";
  public static final String TAG_MODIFIED = "modified";
  public static final String TAG_DESCRIPTION = "description";
  public static final String TAG_ENTITY_TYPE_ID = "entityTypeId";
  public static final String TAG_VARIANT_TYPE = "variantType";
  public static final String TAG_ACCESS = "access";

  public static final String DEFAULT_NAME = "Unnamed Preset";
  public static final String DEFAULT_CATEGORY = "Custom";
  public static final String DEFAULT_VERSION = "1.0.0";
  public static final String DEFAULT_AUTHOR = "Unknown";
  public static final String DEFAULT_DESCRIPTION = "";

  public static final Codec<PresetMetadata> CODEC =
      RecordCodecBuilder.create(
          instance ->
              instance
                  .group(
                      Codec.STRING
                          .optionalFieldOf(TAG_NAME, DEFAULT_NAME)
                          .forGetter(PresetMetadata::name),
                      Codec.STRING
                          .optionalFieldOf(TAG_CATEGORY, DEFAULT_CATEGORY)
                          .forGetter(PresetMetadata::category),
                      Codec.STRING
                          .optionalFieldOf(TAG_VERSION, DEFAULT_VERSION)
                          .forGetter(PresetMetadata::version),
                      Codec.STRING
                          .optionalFieldOf(TAG_AUTHOR, DEFAULT_AUTHOR)
                          .forGetter(PresetMetadata::author),
                      Codec.LONG
                          .optionalFieldOf(TAG_CREATED, 0L)
                          .forGetter(PresetMetadata::created),
                      Codec.LONG
                          .optionalFieldOf(TAG_MODIFIED, 0L)
                          .forGetter(PresetMetadata::modified),
                      Codec.STRING
                          .optionalFieldOf(TAG_DESCRIPTION, DEFAULT_DESCRIPTION)
                          .forGetter(PresetMetadata::description),
                      Codec.STRING
                          .optionalFieldOf(TAG_ENTITY_TYPE_ID, "")
                          .forGetter(
                              metadata ->
                                  metadata.entityTypeId() != null ? metadata.entityTypeId() : ""),
                      Codec.STRING
                          .optionalFieldOf(TAG_VARIANT_TYPE, "")
                          .forGetter(
                              metadata ->
                                  metadata.variantType() != null ? metadata.variantType() : ""),
                      Codec.STRING
                          .optionalFieldOf(TAG_ACCESS, PresetAccess.PUBLIC.name())
                          .xmap(PresetAccess::get, PresetAccess::name)
                          .forGetter(PresetMetadata::access))
                  .apply(
                      instance,
                      (name,
                          category,
                          version,
                          author,
                          created,
                          modified,
                          description,
                          entityTypeId,
                          variantType,
                          access) ->
                          new PresetMetadata(
                              name,
                              category,
                              version,
                              author,
                              created,
                              modified,
                              description,
                              entityTypeId.isEmpty() ? null : entityTypeId,
                              variantType.isEmpty() ? null : variantType,
                              access)));

  public static final StreamCodec<RegistryFriendlyByteBuf, PresetMetadata> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public PresetMetadata decode(RegistryFriendlyByteBuf buffer) {
          String name = buffer.readUtf();
          String category = buffer.readUtf();
          String version = buffer.readUtf();
          String author = buffer.readUtf();
          long created = buffer.readVarLong();
          long modified = buffer.readVarLong();
          String description = buffer.readUtf();
          String entityTypeId = buffer.readUtf();
          String variantType = buffer.readUtf();
          PresetAccess access = PresetAccess.get(buffer.readUtf());
          return new PresetMetadata(
              name,
              category,
              version,
              author,
              created,
              modified,
              description,
              entityTypeId.isEmpty() ? null : entityTypeId,
              variantType.isEmpty() ? null : variantType,
              access);
        }

        @Override
        public void encode(RegistryFriendlyByteBuf buffer, PresetMetadata metadata) {
          buffer.writeUtf(metadata.name());
          buffer.writeUtf(metadata.category());
          buffer.writeUtf(metadata.version());
          buffer.writeUtf(metadata.author());
          buffer.writeVarLong(metadata.created());
          buffer.writeVarLong(metadata.modified());
          buffer.writeUtf(metadata.description());
          buffer.writeUtf(metadata.entityTypeId() != null ? metadata.entityTypeId() : "");
          buffer.writeUtf(metadata.variantType() != null ? metadata.variantType() : "");
          buffer.writeUtf(
              metadata.access() != null ? metadata.access().name() : PresetAccess.PUBLIC.name());
        }
      };

  public PresetMetadata {
    if (name == null || name.isEmpty()) {
      name = DEFAULT_NAME;
    }
    if (category == null || category.isEmpty()) {
      category = DEFAULT_CATEGORY;
    }
    if (version == null || version.isEmpty()) {
      version = DEFAULT_VERSION;
    }
    if (author == null || author.isEmpty()) {
      author = DEFAULT_AUTHOR;
    }
    if (created <= 0) {
      created = System.currentTimeMillis();
    }
    if (modified <= 0) {
      modified = System.currentTimeMillis();
    }
    if (description == null) {
      description = DEFAULT_DESCRIPTION;
    }
    if (access == null) {
      access = PresetAccess.PUBLIC;
    }
  }

  public static PresetMetadata createDefault() {
    return new PresetMetadata(
        DEFAULT_NAME,
        DEFAULT_CATEGORY,
        DEFAULT_VERSION,
        DEFAULT_AUTHOR,
        System.currentTimeMillis(),
        System.currentTimeMillis(),
        DEFAULT_DESCRIPTION,
        null,
        null,
        PresetAccess.PUBLIC);
  }

  public static PresetMetadata getDefault() {
    return createDefault();
  }

  public static PresetMetadata createDefault(String name, String author) {
    return new PresetMetadata(
        name,
        DEFAULT_CATEGORY,
        DEFAULT_VERSION,
        author,
        System.currentTimeMillis(),
        System.currentTimeMillis(),
        DEFAULT_DESCRIPTION,
        null,
        null,
        PresetAccess.PUBLIC);
  }

  public static PresetMetadata fromCompoundTag(CompoundTag tag) {
    if (tag == null || tag.isEmpty()) {
      return createDefault();
    }

    return new PresetMetadata(
        tag.contains(TAG_NAME) ? tag.getString(TAG_NAME).orElse(DEFAULT_NAME) : DEFAULT_NAME,
        tag.contains(TAG_CATEGORY)
            ? tag.getString(TAG_CATEGORY).orElse(DEFAULT_CATEGORY)
            : DEFAULT_CATEGORY,
        tag.contains(TAG_VERSION)
            ? tag.getString(TAG_VERSION).orElse(DEFAULT_VERSION)
            : DEFAULT_VERSION,
        tag.contains(TAG_AUTHOR)
            ? tag.getString(TAG_AUTHOR).orElse(DEFAULT_AUTHOR)
            : DEFAULT_AUTHOR,
        tag.contains(TAG_CREATED)
            ? tag.getLong(TAG_CREATED).orElse(System.currentTimeMillis())
            : System.currentTimeMillis(),
        tag.contains(TAG_MODIFIED)
            ? tag.getLong(TAG_MODIFIED).orElse(System.currentTimeMillis())
            : System.currentTimeMillis(),
        tag.contains(TAG_DESCRIPTION)
            ? tag.getString(TAG_DESCRIPTION).orElse(DEFAULT_DESCRIPTION)
            : DEFAULT_DESCRIPTION,
        tag.contains(TAG_ENTITY_TYPE_ID) ? tag.getString(TAG_ENTITY_TYPE_ID).orElse(null) : null,
        tag.contains(TAG_VARIANT_TYPE) ? tag.getString(TAG_VARIANT_TYPE).orElse(null) : null,
        PresetAccess.get(tag.contains(TAG_ACCESS) ? tag.getString(TAG_ACCESS).orElse(null) : null));
  }

  public static PresetMetadata fromPresetData(CompoundTag presetData) {
    if (presetData == null || !presetData.contains(PresetDataCapable.PRESET_METADATA_TAG)) {
      return createDefault();
    }

    CompoundTag metadataTag =
        presetData.getCompound(PresetDataCapable.PRESET_METADATA_TAG).orElse(new CompoundTag());
    PresetMetadata metadata = fromCompoundTag(metadataTag);

    if (metadata.entityTypeId() == null || metadata.variantType() == null) {
      String entityTypeId = metadata.entityTypeId();
      String variantType = metadata.variantType();

      if (entityTypeId == null && presetData.contains("id")) {
        entityTypeId = presetData.getString("id").orElse(null);
      }

      if (variantType == null && presetData.contains("VariantType")) {
        variantType = presetData.getString("VariantType").orElse(null);
      }

      if (entityTypeId != null || variantType != null) {
        metadata = metadata.withPreviewData(entityTypeId, variantType);
      }
    }

    return metadata;
  }

  public CompoundTag toCompoundTag() {
    CompoundTag tag = new CompoundTag();
    tag.putString(TAG_NAME, this.name);
    tag.putString(TAG_CATEGORY, this.category);
    tag.putString(TAG_VERSION, this.version);
    tag.putString(TAG_AUTHOR, this.author);
    tag.putLong(TAG_CREATED, this.created);
    tag.putLong(TAG_MODIFIED, this.modified);
    tag.putString(TAG_DESCRIPTION, this.description);

    if (this.entityTypeId != null && !this.entityTypeId.isEmpty()) {
      tag.putString(TAG_ENTITY_TYPE_ID, this.entityTypeId);
    }
    if (this.variantType != null && !this.variantType.isEmpty()) {
      tag.putString(TAG_VARIANT_TYPE, this.variantType);
    }
    if (this.access != PresetAccess.PUBLIC) {
      tag.putString(TAG_ACCESS, this.access.name());
    }

    return tag;
  }

  public PresetMetadata withModifiedTime(long modifiedTime) {
    return new PresetMetadata(
        this.name,
        this.category,
        this.version,
        this.author,
        this.created,
        modifiedTime,
        this.description,
        this.entityTypeId,
        this.variantType,
        this.access);
  }

  public PresetMetadata withCurrentModifiedTime() {
    return this.withModifiedTime(System.currentTimeMillis());
  }

  public PresetMetadata withName(String newName) {
    return new PresetMetadata(
        newName,
        this.category,
        this.version,
        this.author,
        this.created,
        System.currentTimeMillis(),
        this.description,
        this.entityTypeId,
        this.variantType,
        this.access);
  }

  public PresetMetadata withCategory(String newCategory) {
    return new PresetMetadata(
        this.name,
        newCategory,
        this.version,
        this.author,
        this.created,
        System.currentTimeMillis(),
        this.description,
        this.entityTypeId,
        this.variantType,
        this.access);
  }

  public PresetMetadata withVersion(String newVersion) {
    return new PresetMetadata(
        this.name,
        this.category,
        newVersion,
        this.author,
        this.created,
        System.currentTimeMillis(),
        this.description,
        this.entityTypeId,
        this.variantType,
        this.access);
  }

  public PresetMetadata withAuthor(String newAuthor) {
    return new PresetMetadata(
        this.name,
        this.category,
        this.version,
        newAuthor,
        this.created,
        System.currentTimeMillis(),
        this.description,
        this.entityTypeId,
        this.variantType,
        this.access);
  }

  public PresetMetadata withDescription(String newDescription) {
    return new PresetMetadata(
        this.name,
        this.category,
        this.version,
        this.author,
        this.created,
        System.currentTimeMillis(),
        newDescription,
        this.entityTypeId,
        this.variantType,
        this.access);
  }

  public PresetMetadata withAccess(PresetAccess newAccess) {
    return new PresetMetadata(
        this.name,
        this.category,
        this.version,
        this.author,
        this.created,
        System.currentTimeMillis(),
        this.description,
        this.entityTypeId,
        this.variantType,
        newAccess);
  }

  public PresetMetadata withPreviewData(String newEntityTypeId, String newVariantType) {
    return new PresetMetadata(
        this.name,
        this.category,
        this.version,
        this.author,
        this.created,
        this.modified,
        this.description,
        newEntityTypeId,
        newVariantType,
        this.access);
  }
}

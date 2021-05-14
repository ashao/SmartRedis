#include "metadatafield.h"

using namespace SmartRedis;

MetadataField::MetadataField(const std::string& name,
                             MetaDataType type)
{
    this->_name = name;
    this->_type = type;
}

MetadataField::MetadataField(const std::string& name,
                             const std::string_view& serial_string)
{
    this->_name = name;
    this->_type = this->get_type_from_buffer(serial_string);
}


std::string MetadataField::name()
{
    return this->_name;
}

MetaDataType MetadataField::type()
{
    return this->_type;
}

MetaDataType MetadataField::get_type_from_buffer(
    const std::string_view& buf)
{
    void* data = (void*)(buf.data());
    int8_t type = *((int8_t*)data);
    return (MetaDataType)type;
}
